module Commands.ResignDelegation (
  ResignDelegationCommand (..),
  resignDelegationCommandParser,
  runResignDelegationCommand,
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
 )
import PlutusTx (fromData)

data ResignDelegationCommand = ResignDelegationCommand
  { utxoFile :: FilePath
  , delegationCert :: FilePath
  , outDir :: FilePath
  }

resignDelegationCommandParser :: ParserInfo ResignDelegationCommand
resignDelegationCommandParser = info parser description
  where
    description :: InfoMod ResignDelegationCommand
    description =
      progDesc "Spend the cold NFT to remove a delegation user from the datum."

    parser :: Parser ResignDelegationCommand
    parser =
      ResignDelegationCommand
        <$> utxoFileParser
        <*> delegationFileParser
        <*> outDirParser

runResignDelegationCommand :: ResignDelegationCommand -> IO ()
runResignDelegationCommand ResignDelegationCommand{..} = do
  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile
  delegationUser <- readIdentityFromPEMFile' delegationCert

  TxOut (AddressInEra _ address) value txOutDatum _ <- case utxoResult of
    Left err -> do
      error $ "Failed to read utxo file: " <> show err
    Right u -> pure u

  ColdLockDatum{..} <- case txOutDatum of
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

  writePlutusDataToFile outDir "redeemer.json" $ ResignDelegation delegationUser
  let datum =
        ColdLockDatum
          { delegationUsers = filter (/= delegationUser) delegationUsers
          , ..
          }
  writePlutusDataToFile outDir "datum.json" datum
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
