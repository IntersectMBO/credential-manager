module Commands.InitCold (
  InitColdCommand (..),
  initColdCommandParser,
  runInitColdCommand,
) where

import Cardano.Api (NetworkId, StakeAddressReference (..), TxIn)
import Commands.Common (
  StakeCredentialFile,
  delegationCertParser,
  membershipCertParser,
  networkIdParser,
  outDirParser,
  readIdentityFromPEMFile',
  readStakeAddressFile,
  runCommand,
  seedInputParser,
  stakeCredentialFileParser,
  writeBech32ToFile,
  writeHexBytesToFile,
  writePlutusDataToFile,
  writeScriptToFile,
 )
import Control.Applicative (Alternative (..), optional)
import CredentialManager.Orchestrator.InitCold
import Data.Foldable (Foldable (..))
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  action,
  help,
  info,
  long,
  metavar,
  progDesc,
  strOption,
 )

data InitColdCommand = InitColdCommand
  { seedInput :: TxIn
  , networkId :: NetworkId
  , caCertFile :: FilePath
  , membershipCertFiles :: [FilePath]
  , delegationCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  , outDir :: FilePath
  }

initColdCommandParser :: ParserInfo InitColdCommand
initColdCommandParser = info parser description
  where
    description :: InfoMod InitColdCommand
    description = progDesc "Initialize the cold CC credential scripts."

    parser :: Parser InitColdCommand
    parser =
      InitColdCommand
        <$> seedInputParser
        <*> networkIdParser
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

runInitColdCommand :: InitColdCommand -> IO ()
runInitColdCommand InitColdCommand{..} = do
  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile
  certificateAuthority <- readIdentityFromPEMFile' caCertFile
  membershipUsers <- traverse readIdentityFromPEMFile' membershipCertFiles
  delegationUsers <- traverse readIdentityFromPEMFile' delegationCertFiles
  let inputs = InitColdInputs{..}
  InitColdOutputs{..} <- runCommand initCold inputs \case
    SeedTxIxTooLarge -> "The seed input has too large of an index. It must be less than 256."
    MembershipTooSmall -> "At least 3 membership users are required"
    DuplicateMembershipCertificates cert ->
      "Multiple membership users have the same certificate hash " <> show cert
    DuplicateMembershipPubKeyHash key ->
      "Multiple membership users have the same public key hash " <> show key
    DelegationTooSmall -> "At least 3 delegation users are required"
    DuplicateDelegationCertificates cert ->
      "Multiple delegation users have the same certificate hash " <> show cert
    DuplicateDelegationPubKeyHash key ->
      "Multiple delegation users have the same public key hash " <> show key
  writeScriptToFile outDir "minting.plutus" mintingScript
  writeHexBytesToFile outDir "minting.plutus.hash" mintingScriptHash
  writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
  writeHexBytesToFile outDir "nft-token-name" coldNFTAssetName
  writeScriptToFile outDir "credential.plutus" credentialScript
  writeHexBytesToFile outDir "credential.plutus.hash" credentialScriptHash
  writeScriptToFile outDir "nft.plutus" nftScript
  writeHexBytesToFile outDir "nft.plutus.hash" nftScriptHash
  writeBech32ToFile outDir "nft.addr" nftScriptAddress
  writePlutusDataToFile outDir "nft.datum.json" nftDatum
