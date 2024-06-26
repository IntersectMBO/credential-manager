module CredentialManager.Orchestrator.Common where

import Cardano.Api (
  Address,
  AddressInEra (..),
  ConwayEra,
  CtxUTxO,
  PlutusScriptVersion (..),
  Script (..),
  ScriptData,
  ShelleyAddr,
  TxOut,
  TxOutDatum (..),
  UTxO (..),
  getScriptData,
 )
import Cardano.Api.Byron (Address (..))
import Cardano.Api.Ledger (Credential (..))
import Cardano.Api.Shelley (
  Address (..),
  PlutusScript (..),
  toPlutusData,
 )
import CredentialManager.Api (CertificateHash, Identity (..))
import Data.List (group, sort)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import PlutusLedgerApi.V3 (
  FromData (..),
  PubKeyHash,
  fromData,
  serialiseCompiledCode,
 )
import PlutusTx (CompiledCode)

getScriptAddress
  :: e
  -> e
  -> AddressInEra ConwayEra
  -> Either e (Address ShelleyAddr)
getScriptAddress byron payment (AddressInEra _ address) = case address of
  ByronAddress{} -> Left byron
  ShelleyAddress _ (KeyHashObj _) _ -> Left payment
  addr@(ShelleyAddress _ (ScriptHashObj _) _) -> Right addr

decodeDatum :: (FromData a) => e -> ScriptData -> Either e a
decodeDatum invalid = maybe (Left invalid) Right . fromData . toPlutusData

getInlineDatum :: e -> e -> TxOutDatum CtxUTxO ConwayEra -> Either e ScriptData
getInlineDatum missing nonInline = \case
  TxOutDatumInline _ datum -> Right $ getScriptData datum
  TxOutDatumNone -> Left missing
  TxOutDatumHash _ _ -> Left nonInline

serialiseScript :: PlutusScriptVersion v -> CompiledCode a -> Script v
serialiseScript v =
  PlutusScript v . PlutusScriptSerialised . serialiseCompiledCode

validateGroup
  :: e
  -> (CertificateHash -> e)
  -> (PubKeyHash -> e)
  -> [Identity]
  -> Either e ()
validateGroup empty _ _ [] = Left empty
validateGroup _ duplicateCert duplicateKey list = do
  checkDuplicates duplicateCert certificateHash list
  checkDuplicates duplicateKey pubKeyHash list
  where
    checkDuplicates err f =
      maybe (Right ()) (Left . err)
        . listToMaybe
        . concat
        . filter ((> 1) . length)
        . group
        . sort
        . fmap f

validate :: e -> Bool -> Either e ()
validate _ True = Right ()
validate e False = Left e

extractOutput :: a -> a -> UTxO ConwayEra -> Either a (TxOut CtxUTxO ConwayEra)
extractOutput emptyErr ambiguousErr (UTxO utxo) = case Map.elems utxo of
  [] -> Left emptyErr
  [out] -> Right out
  _ -> Left ambiguousErr
