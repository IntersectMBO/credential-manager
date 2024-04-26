module Commands.Vote (
  VoteCommand (..),
  voteCommandParser,
  runVoteCommand,
) where

import Cardano.Api (
  AddressInEra (..),
  AsType (..),
  ConwayEra,
  CtxUTxO,
  File (..),
  PlutusScriptVersion (..),
  Script (..),
  ShelleyAddr,
  TxOut (..),
  TxOutDatum (..),
  getScriptData,
  hashScript,
  readFileTextEnvelope,
  txOutValueToValue,
 )
import Cardano.Api.Byron (Address (..))
import Cardano.Api.Ledger (
  Anchor (..),
  AnchorData,
  Credential (KeyHashObj, ScriptHashObj),
  GovActionId (GovActionId),
  SafeHash,
  StandardCrypto,
  StrictMaybe (..),
  Url,
  Vote (..),
  Voter (..),
  VotingProcedure (VotingProcedure),
  extractHash,
  hashToBytes,
 )
import Cardano.Api.Shelley (
  Address (..),
  toPlutusData,
  toShelleyScriptHash,
 )
import Cardano.Ledger.Conway.Governance (GovActionIx (GovActionIx))
import Cardano.Ledger.TxIn (TxId (..))
import Commands.Common (
  hotCredentialScriptFileParser,
  metadataHashParser,
  metadataUrlParser,
  outDirParser,
  readSafeHash,
  utxoFileParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
  writeVoteToFile,
 )
import CredentialManager.Api (
  HotLockDatum (..),
 )
import qualified CredentialManager.Api as Api
import Data.Aeson (eitherDecodeFileStrict)
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
import PlutusLedgerApi.V3 (
  GovernanceActionId (GovernanceActionId),
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3
import PlutusTx (fromData)

data VoteCommand = VoteCommand
  { utxoFile :: FilePath
  , hotCredentialScriptFile :: FilePath
  , govActionId :: GovActionId StandardCrypto
  , vote :: Vote
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
  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile

  hotCredentialScriptResult <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File hotCredentialScriptFile)

  hotCredentialScript <- case hotCredentialScriptResult of
    Left err -> do
      error $ "Failed to read hot credential script file: " <> show err
    Right script -> pure $ PlutusScript PlutusScriptV3 script

  TxOut (AddressInEra _ address) value txOutDatum _ <- case utxoResult of
    Left err -> do
      error $ "Failed to read utxo file: " <> show err
    Right u -> pure u

  datum <- case txOutDatum of
    TxOutDatumInline _ datum ->
      case fromData @HotLockDatum $ toPlutusData $ getScriptData datum of
        Nothing -> error "Unable to decode datum in UTxO"
        Just d -> pure d
    TxOutDatumNone -> error "No datum in utxo"
    TxOutDatumHash _ _ -> error "Inline datum required in utxo"

  shelleyAddress <-
    ( case address of
        ByronAddress{} -> error "UTxO address is a Byron address."
        ShelleyAddress _ (KeyHashObj _) _ -> error "UTxO address is a payment key address."
        addr@(ShelleyAddress _ (ScriptHashObj _) _) -> pure addr
    )
      :: IO (Address ShelleyAddr)

  let plutusGovActionId = case govActionId of
        GovActionId (TxId txId) (GovActionIx ix) ->
          GovernanceActionId
            (PV3.TxId $ toBuiltin $ hashToBytes $ extractHash txId)
            (fromIntegral ix)
  let plutusVote = case vote of
        VoteYes -> PV3.VoteYes
        VoteNo -> PV3.VoteNo
        Abstain -> PV3.Abstain
  writePlutusDataToFile outDir "redeemer.json" $
    Api.Vote plutusGovActionId plutusVote
  writePlutusDataToFile outDir "datum.json" datum
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
  let votingProcedure = VotingProcedure vote (SJust $ Anchor metadataUrl metadataHash)
  let hotCredentialScriptHash = hashScript hotCredentialScript
  let voter = CommitteeVoter $ ScriptHashObj $ toShelleyScriptHash hotCredentialScriptHash
  writeVoteToFile outDir "vote" voter govActionId votingProcedure
