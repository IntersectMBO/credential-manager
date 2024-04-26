module Commands.RotateHot (
  RotateHotCommand (..),
  rotateHotCommandParser,
  runRotateHotCommand,
) where

import Cardano.Api (
  AddressInEra (..),
  ConwayEra,
  CtxUTxO,
  ShelleyAddr,
  TxOut (..),
  TxOutDatum (..),
  getScriptData,
  txOutValueToValue,
 )
import Cardano.Api.Byron (Address (..))
import Cardano.Api.Ledger (
  Credential (KeyHashObj, ScriptHashObj),
 )
import Cardano.Api.Shelley (
  Address (..),
  toPlutusData,
 )
import Commands.Common (
  outDirParser,
  readIdentityFromPEMFile',
  utxoFileParser,
  votingCertParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Api (
  HotLockDatum (..),
  HotLockRedeemer (..),
 )
import Data.Aeson (eitherDecodeFileStrict)
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
  some,
 )
import PlutusTx (fromData)

data RotateHotCommand = RotateHotCommand
  { utxoFile :: FilePath
  , votingCerts :: [FilePath]
  , outDir :: FilePath
  }

rotateHotCommandParser :: ParserInfo RotateHotCommand
rotateHotCommandParser = info parser description
  where
    description :: InfoMod RotateHotCommand
    description =
      progDesc "Spend the hot NFT to rotate voting keys in the datum."

    parser :: Parser RotateHotCommand
    parser =
      RotateHotCommand
        <$> utxoFileParser
        <*> some votingCertParser
        <*> outDirParser

runRotateHotCommand :: RotateHotCommand -> IO ()
runRotateHotCommand RotateHotCommand{..} = do
  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile
  votingUsers <- traverse readIdentityFromPEMFile' votingCerts

  TxOut (AddressInEra _ address) value txOutDatum _ <- case utxoResult of
    Left err -> do
      error $ "Failed to read utxo file: " <> show err
    Right u -> pure u

  HotLockDatum _ <- case txOutDatum of
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

  writePlutusDataToFile outDir "redeemer.json" RotateHot
  writePlutusDataToFile outDir "datum.json" HotLockDatum{..}
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
