module CredentialManager.Orchestrator.ResignDelegation where

import Cardano.Api (
  Address,
  ConwayEra,
  CtxUTxO,
  ShelleyAddr,
  TxOut (..),
  Value,
  txOutValueToValue,
 )
import CredentialManager.Api
import CredentialManager.Orchestrator.Common (
  decodeDatum,
  getInlineDatum,
  getScriptAddress,
  validate,
 )
import GHC.Generics (Generic)

data ResignDelegationInputs = ResignDelegationInputs
  { resignee :: Identity
  , scriptUtxo :: TxOut CtxUTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data ResignDelegationOutputs = ResignDelegationOutputs
  { redeemer :: ColdLockRedeemer
  , outputDatum :: ColdLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  }
  deriving (Show, Eq, Generic)

data ResignDelegationError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  | ResigneeNotInDelegationGroup
  | EmptyDelegation
  deriving (Show, Eq, Generic)

resignDelegation
  :: ResignDelegationInputs -> Either ResignDelegationError ResignDelegationOutputs
resignDelegation ResignDelegationInputs{..} = do
  let TxOut address inputValue txOutDatum _ = scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let oldDelegation = delegationUsers inputDatum
  validate ResigneeNotInDelegationGroup $ resignee `elem` oldDelegation
  let newDelegation = filter (/= resignee) oldDelegation
  validate EmptyDelegation $ not $ null newDelegation
  let outputDatum = inputDatum{delegationUsers = newDelegation}
  let redeemer = ResignDelegation resignee
  let outputValue = txOutValueToValue inputValue
  pure ResignDelegationOutputs{..}
