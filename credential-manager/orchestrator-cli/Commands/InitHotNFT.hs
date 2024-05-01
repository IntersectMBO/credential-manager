module Commands.InitHotNFT (
  InitHotNFTCommand (..),
  initHotNFTCommandParser,
  runInitHotNFTCommand,
) where

import Cardano.Api (
  NetworkId,
  PolicyId,
  StakeAddressReference (..),
 )
import Commands.Common (
  StakeCredentialFile,
  hotCredentialScriptFileParser,
  networkIdParser,
  outDirParser,
  policyIdParser,
  readFilePlutusV3Script,
  readIdentityFromPEMFile',
  readStakeAddressFile,
  runCommand,
  stakeCredentialFileParser,
  votingCertParser,
  writeBech32ToFile,
  writeHexBytesToFile,
  writePlutusDataToFile,
  writeScriptToFile,
 )
import CredentialManager.Orchestrator.InitHotNFT
import Data.Foldable (Foldable (..))
import Options.Applicative (
  Alternative (some),
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  help,
  info,
  long,
  optional,
  progDesc,
 )

data InitHotNFTCommand = InitHotNFTCommand
  { networkId :: NetworkId
  , coldNFTPolicyId :: PolicyId
  , hotCredentialScriptFile :: FilePath
  , votingCerts :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  , outDir :: FilePath
  }

initHotNFTCommandParser :: ParserInfo InitHotNFTCommand
initHotNFTCommandParser = info parser description
  where
    description :: InfoMod InitHotNFTCommand
    description =
      progDesc "Initialize the hot NFT lock script by sending an NFT to it."

    parser :: Parser InitHotNFTCommand
    parser =
      InitHotNFTCommand
        <$> networkIdParser
        <*> policyIdParser coldNFTPolicyIdInfo
        <*> hotCredentialScriptFileParser
        <*> some votingCertParser
        <*> optional stakeCredentialFileParser
        <*> outDirParser

coldNFTPolicyIdInfo :: Mod OptionFields PolicyId
coldNFTPolicyIdInfo =
  fold
    [ long "cold-nft-policy-id"
    , help "The minting policy ID of the cold NFT."
    ]

runInitHotNFTCommand :: InitHotNFTCommand -> IO ()
runInitHotNFTCommand InitHotNFTCommand{..} = do
  hotCredentialScript <- readFilePlutusV3Script hotCredentialScriptFile
  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile
  votingUsers <- traverse readIdentityFromPEMFile' votingCerts
  let inputs = InitHotNFTInputs{..}
  InitHotNFTOutputs{..} <- runCommand initHotNFT inputs \case
    EmptyVoting -> "No voting users specified"
    DuplicateVotingCertificates cert ->
      "Multiple voting users have the same certificate hash " <> show cert
    DuplicateVotingPubKeyHash key ->
      "Multiple voting users have the same public key hash " <> show key
  writeScriptToFile outDir "script.plutus" script
  writeHexBytesToFile outDir "script.hash" scriptHash
  writeBech32ToFile outDir "script.addr" scriptAddress
  writePlutusDataToFile outDir "datum.json" initialDatum
