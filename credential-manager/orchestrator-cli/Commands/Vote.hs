module Commands.Vote (
  VoteCommand (..),
  voteCommandParser,
  runVoteCommand,
) where

import Cardano.Api.Ledger (
  AnchorData,
  GovActionId (..),
  SafeHash,
  StandardCrypto,
  Url,
  Vote (..),
 )
import Cardano.Ledger.Conway.Governance (GovActionIx (GovActionIx))
import Cardano.Ledger.TxIn (TxId (..))
import Commands.Common (
  hotCredentialScriptFileParser,
  metadataHashParser,
  metadataUrlParser,
  outDirParser,
  readFilePlutusV3Script,
  readFileTxOut,
  readSafeHash,
  runCommand,
  utxoFileParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
  writeVoteToFile,
 )
import CredentialManager.Orchestrator.Vote
import Data.Foldable (Foldable (..), asum)
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  ReadM,
  auto,
  flag',
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
 )

data VoteCommand = VoteCommand
  { utxoFile :: FilePath
  , hotCredentialScriptFile :: FilePath
  , govActionId :: GovActionId StandardCrypto
  , vote_ :: Vote
  , metadataUrl :: Url
  , metadataHash :: SafeHash StandardCrypto AnchorData
  , outDir :: FilePath
  }

voteCommandParser :: ParserInfo VoteCommand
voteCommandParser = info parser description
  where
    description :: InfoMod VoteCommand
    description =
      progDesc "Spend the hot NFT to vote a hot credential."

    parser :: Parser VoteCommand
    parser =
      VoteCommand
        <$> utxoFileParser
        <*> hotCredentialScriptFileParser
        <*> govActionIdParser
        <*> voteParser
        <*> metadataUrlParser metadataInfo
        <*> metadataHashParser
        <*> outDirParser

govActionIdParser :: Parser (GovActionId StandardCrypto)
govActionIdParser = GovActionId <$> txIdParser <*> govActionIxParser

txIdParser :: Parser (TxId StandardCrypto)
txIdParser =
  option readTxId $
    fold
      [ long "governance-action-tx-id"
      , metavar "TX_ID"
      , help "ID of the transaction containing the governance action."
      ]

readTxId :: ReadM (TxId StandardCrypto)
readTxId = TxId <$> readSafeHash

govActionIxParser :: Parser GovActionIx
govActionIxParser =
  option readGovActionIx $
    fold
      [ long "governance-action-tx-id"
      , metavar "WORD32"
      , help "Index of the governance action withing the transaction."
      ]

readGovActionIx :: ReadM GovActionIx
readGovActionIx = GovActionIx <$> auto

voteParser :: Parser Vote
voteParser =
  asum
    [ flag' VoteYes $ long "yes"
    , flag' VoteNo $ long "no"
    , flag' Abstain $ long "abstain"
    ]

metadataInfo :: Mod OptionFields Url
metadataInfo =
  fold
    [ help "URL of the governance metadata document giving rationale for the vote."
    ]

runVoteCommand :: VoteCommand -> IO ()
runVoteCommand VoteCommand{..} = do
  hotCredentialScript <- readFilePlutusV3Script hotCredentialScriptFile
  scriptUtxo <- readFileTxOut utxoFile
  let inputs = VoteInputs{..}
  VoteOutputs{..} <- runCommand vote inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
  writeVoteToFile outDir "vote" votingProcedures
