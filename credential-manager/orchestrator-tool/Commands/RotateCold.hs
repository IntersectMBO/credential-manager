module Commands.RotateCold (
  RotateColdCommand (..),
  rotateColdCommandParser,
  runRotateColdCommand,
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
  delegationFileParser,
  membershipFileParser,
  outDirParser,
  readIdentityFromPEMFile',
  utxoFileParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Api (
  ColdLockDatum (..),
  ColdLockRedeemer (..),
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

data RotateColdCommand = RotateColdCommand
  { utxoFile :: FilePath
  , membershipCerts :: [FilePath]
  , delegationCerts :: [FilePath]
  , outDir :: FilePath
  }

rotateColdCommandParser :: ParserInfo RotateColdCommand
rotateColdCommandParser = info parser description
  where
    description :: InfoMod RotateColdCommand
    description =
      progDesc
        "Spend the cold NFT to rotate membership and delegation keys in the datum."

    parser :: Parser RotateColdCommand
    parser =
      RotateColdCommand
        <$> utxoFileParser
        <*> some membershipFileParser
        <*> some delegationFileParser
        <*> outDirParser

runRotateColdCommand :: RotateColdCommand -> IO ()
runRotateColdCommand RotateColdCommand{..} = do
  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile
  membershipUsers <- traverse readIdentityFromPEMFile' membershipCerts
  delegationUsers <- traverse readIdentityFromPEMFile' delegationCerts

  TxOut (AddressInEra _ address) value txOutDatum _ <- case utxoResult of
    Left err -> do
      error $ "Failed to read utxo file: " <> show err
    Right u -> pure u

  ColdLockDatum certificateAuthority _ _ <- case txOutDatum of
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

  writePlutusDataToFile outDir "redeemer.json" RotateCold
  writePlutusDataToFile outDir "datum.json" ColdLockDatum{..}
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
