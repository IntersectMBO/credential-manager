module Commands.Resign (
  ResignCommand (..),
  resignCommandParser,
  runResignCommand,
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
  Anchor (Anchor),
  AnchorData,
  ConwayGovCert (ConwayResignCommitteeColdKey),
  ConwayTxCert (ConwayTxCertGov),
  Credential (KeyHashObj, ScriptHashObj),
  SafeHash,
  StandardCrypto,
  StrictMaybe (..),
  Url,
 )
import Cardano.Api.Shelley (
  Address (..),
  toPlutusData,
  toShelleyScriptHash,
 )
import Commands.Common (
  coldCredentialScriptFileParser,
  metadataHashParser,
  metadataUrlParser,
  outDirParser,
  utxoFileParser,
  writeCertificateToFile,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Api (
  ColdLockDatum (..),
  ColdLockRedeemer (..),
 )
import Data.Aeson (eitherDecodeFileStrict)
import Data.Foldable (Foldable (..))
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  help,
  info,
  progDesc,
 )
import PlutusTx (fromData)

data ResignCommand = ResignCommand
  { utxoFile :: FilePath
  , coldCredentialScriptFile :: FilePath
  , metadataUrl :: Url
  , metadataHash :: SafeHash StandardCrypto AnchorData
  , outDir :: FilePath
  }

resignCommandParser :: ParserInfo ResignCommand
resignCommandParser = info parser description
  where
    description :: InfoMod ResignCommand
    description = progDesc "Spend the cold NFT to resign from the committee."

    parser :: Parser ResignCommand
    parser =
      ResignCommand
        <$> utxoFileParser
        <*> coldCredentialScriptFileParser
        <*> metadataUrlParser metadataInfo
        <*> metadataHashParser
        <*> outDirParser

metadataInfo :: Mod OptionFields Url
metadataInfo =
  fold
    [ help "URL of the governance metadata document giving context for resignation."
    ]

runResignCommand :: ResignCommand -> IO ()
runResignCommand ResignCommand{..} = do
  coldCredentialScriptResult <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File coldCredentialScriptFile)

  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile

  coldCredentialScript <- case coldCredentialScriptResult of
    Left err -> do
      error $ "Failed to read cold credential script file: " <> show err
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
  writePlutusDataToFile outDir "redeemer.json" ResignCold
  writePlutusDataToFile outDir "datum.json" datum
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
  writeCertificateToFile outDir "resignHot.cert" $
    ConwayCertificate ConwayEraOnwardsConway $
      ConwayTxCertGov $
        ConwayResignCommitteeColdKey
          (ScriptHashObj $ toShelleyScriptHash coldCredentialScriptHash)
          (SJust $ Anchor metadataUrl metadataHash)
