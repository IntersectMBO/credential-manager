module Commands.Authorize (
  AuthorizeCommand (..),
  authorizeCommandParser,
  runAuthorizeCommand,
) where

import Cardano.Api (
  AddressInEra (..),
  AsType (..),
  Certificate (ConwayCertificate),
  ConwayEra,
  ConwayEraOnwards (ConwayEraOnwardsConway),
  CtxUTxO,
  File (..),
  PlutusScriptVersion (..),
  Script (..),
  SerialiseAsRawBytes (..),
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
  ConwayGovCert (ConwayAuthCommitteeHotKey),
  ConwayTxCert (ConwayTxCertGov),
  Credential (KeyHashObj, ScriptHashObj),
 )
import Cardano.Api.Shelley (
  Address (..),
  toPlutusData,
  toShelleyScriptHash,
 )
import Commands.Common (
  coldCredentialScriptFileParser,
  hotCredentialScriptFileParser,
  outDirParser,
  utxoFileParser,
  writeCertificateToFile,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Api (
  ColdLockDatum (..),
  ColdLockRedeemer (AuthorizeHot),
 )
import Data.Aeson (eitherDecodeFileStrict)
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )
import PlutusLedgerApi.V3 (
  Credential (..),
  HotCommitteeCredential (..),
  ScriptHash (..),
  toBuiltin,
 )
import PlutusTx (fromData)

data AuthorizeCommand = AuthorizeCommand
  { utxoFile :: FilePath
  , coldCredentialScriptFile :: FilePath
  , hotCredentialScriptFile :: FilePath
  , outDir :: FilePath
  }

authorizeCommandParser :: ParserInfo AuthorizeCommand
authorizeCommandParser = info parser description
  where
    description :: InfoMod AuthorizeCommand
    description =
      progDesc "Spend the cold NFT to authorize a hot credential."

    parser :: Parser AuthorizeCommand
    parser =
      AuthorizeCommand
        <$> utxoFileParser
        <*> coldCredentialScriptFileParser
        <*> hotCredentialScriptFileParser
        <*> outDirParser

runAuthorizeCommand :: AuthorizeCommand -> IO ()
runAuthorizeCommand AuthorizeCommand{..} = do
  coldCredentialScriptResult <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File coldCredentialScriptFile)

  hotCredentialScriptResult <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File hotCredentialScriptFile)

  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile

  coldCredentialScript <- case coldCredentialScriptResult of
    Left err -> do
      error $ "Failed to read cold credential script file: " <> show err
    Right script -> pure $ PlutusScript PlutusScriptV3 script

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
      case fromData @ColdLockDatum $ toPlutusData $ getScriptData datum of
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

  let coldCredentialScriptHash = hashScript coldCredentialScript
  let hotCredentialScriptHash = hashScript hotCredentialScript
  let hotCredentialScriptHashPlutus =
        ScriptHash $
          toBuiltin $
            serialiseToRawBytes hotCredentialScriptHash
  let hotCredential =
        HotCommitteeCredential $ ScriptCredential hotCredentialScriptHashPlutus
  writePlutusDataToFile outDir "redeemer.json" $ AuthorizeHot hotCredential
  writePlutusDataToFile outDir "datum.json" datum
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
  writeCertificateToFile outDir "authorizeHot.cert" $
    ConwayCertificate ConwayEraOnwardsConway $
      ConwayTxCertGov $
        ConwayAuthCommitteeHotKey
          (ScriptHashObj $ toShelleyScriptHash coldCredentialScriptHash)
          (ScriptHashObj $ toShelleyScriptHash hotCredentialScriptHash)
