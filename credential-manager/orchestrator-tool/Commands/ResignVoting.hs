module Commands.ResignVoting (
  ResignVotingCommand (..),
  resignVotingCommandParser,
  runResignVotingCommand,
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
 )
import PlutusTx (fromData)

data ResignVotingCommand = ResignVotingCommand
  { utxoFile :: FilePath
  , votingCert :: FilePath
  , outDir :: FilePath
  }

resignVotingCommandParser :: ParserInfo ResignVotingCommand
resignVotingCommandParser = info parser description
  where
    description :: InfoMod ResignVotingCommand
    description =
      progDesc "Spend the hot NFT to remove a voting user from the datum."

    parser :: Parser ResignVotingCommand
    parser =
      ResignVotingCommand
        <$> utxoFileParser
        <*> votingCertParser
        <*> outDirParser

runResignVotingCommand :: ResignVotingCommand -> IO ()
runResignVotingCommand ResignVotingCommand{..} = do
  utxoResult <- eitherDecodeFileStrict @(TxOut CtxUTxO ConwayEra) utxoFile
  votingUser <- readIdentityFromPEMFile' votingCert

  TxOut (AddressInEra _ address) value txOutDatum _ <- case utxoResult of
    Left err -> do
      error $ "Failed to read utxo file: " <> show err
    Right u -> pure u

  HotLockDatum{..} <- case txOutDatum of
    TxOutDatumInline _ datum ->
      case fromData $ toPlutusData $ getScriptData datum of
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

  writePlutusDataToFile outDir "redeemer.json" $ ResignVoting votingUser
  let datum =
        HotLockDatum
          { votingUsers = filter (/= votingUser) votingUsers
          }
  writePlutusDataToFile outDir "datum.json" datum
  writeTxOutValueToFile outDir "value" shelleyAddress $ txOutValueToValue value
