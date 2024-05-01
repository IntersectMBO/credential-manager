module Commands.InitColdNFT (
  InitColdNFTCommand (..),
  initColdNFTCommandParser,
  runInitColdNFTCommand,
) where

import Cardano.Api (NetworkId, StakeAddressReference (..))
import Commands.Common (
  StakeCredentialFile,
  coldCredentialScriptFileParser,
  delegationCertParser,
  membershipCertParser,
  networkIdParser,
  outDirParser,
  readFilePlutusV3Script,
  readIdentityFromPEMFile',
  readStakeAddressFile,
  runCommand,
  stakeCredentialFileParser,
  writeBech32ToFile,
  writeHexBytesToFile,
  writePlutusDataToFile,
  writeScriptToFile,
 )
import CredentialManager.Orchestrator.InitColdNFT
import Data.Foldable (Foldable (..))
import Options.Applicative (
  Alternative (some),
  InfoMod,
  Parser,
  ParserInfo,
  action,
  help,
  info,
  long,
  metavar,
  optional,
  progDesc,
  strOption,
 )

data InitColdNFTCommand = InitColdNFTCommand
  { networkId :: NetworkId
  , coldCredentialScriptFile :: FilePath
  , caCertFile :: FilePath
  , membershipCertFiles :: [FilePath]
  , delegationCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  , outDir :: FilePath
  }

initColdNFTCommandParser :: ParserInfo InitColdNFTCommand
initColdNFTCommandParser = info parser description
  where
    description :: InfoMod InitColdNFTCommand
    description =
      progDesc "Initialize the cold NFT lock script by sending an NFT to it."

    parser :: Parser InitColdNFTCommand
    parser =
      InitColdNFTCommand
        <$> networkIdParser
        <*> coldCredentialScriptFileParser
        <*> caCertFileParser
        <*> some membershipCertParser
        <*> some delegationCertParser
        <*> optional stakeCredentialFileParser
        <*> outDirParser

caCertFileParser :: Parser FilePath
caCertFileParser =
  strOption $
    fold
      [ long "ca-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the root CA certificate PEM file."
      , action "file"
      ]

runInitColdNFTCommand :: InitColdNFTCommand -> IO ()
runInitColdNFTCommand InitColdNFTCommand{..} = do
  coldCredentialScript <- readFilePlutusV3Script coldCredentialScriptFile
  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile
  certificateAuthority <- readIdentityFromPEMFile' caCertFile
  membershipUsers <- traverse readIdentityFromPEMFile' membershipCertFiles
  delegationUsers <- traverse readIdentityFromPEMFile' delegationCertFiles
  let inputs = InitColdNFTInputs{..}
  InitColdNFTOutputs{..} <- runCommand initColdNFT inputs \case
    EmptyMembership -> "No membership users specified"
    DuplicateMembershipCertificates cert ->
      "Multiple membership users have the same certificate hash " <> show cert
    DuplicateMembershipPubKeyHash key ->
      "Multiple membership users have the same public key hash " <> show key
    EmptyDelegation -> "No delegation users specified"
    DuplicateDelegationCertificates cert ->
      "Multiple delegation users have the same certificate hash " <> show cert
    DuplicateDelegationPubKeyHash key ->
      "Multiple delegation users have the same public key hash " <> show key
  writeScriptToFile outDir "script.plutus" script
  writeHexBytesToFile outDir "script.hash" scriptHash
  writeBech32ToFile outDir "script.addr" scriptAddress
  writePlutusDataToFile outDir "datum.json" initialDatum
