module CredentialManager.Orchestrator.ResignMembership where

import Cardano.Api (
  Address,
  ConwayEra,
  ShelleyAddr,
  TxOut (..),
  UTxO,
  Value,
  txOutValueToValue,
 )
import CredentialManager.Api
import CredentialManager.Orchestrator.Common (
  decodeDatum,
  extractOutput,
  getInlineDatum,
  getScriptAddress,
  validate,
 )
import GHC.Generics (Generic)

data ResignMembershipInputs = ResignMembershipInputs
  { resignee :: Identity
  , scriptUtxo :: UTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data ResignMembershipOutputs = ResignMembershipOutputs
  { redeemer :: ColdLockRedeemer
  , outputDatum :: ColdLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  }
  deriving (Show, Eq, Generic)

data ResignMembershipError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  | ResigneeNotInMembershipGroup
  | EmptyMembership
  | EmptyUTxO
  | AmbiguousUTxO
  deriving (Show, Eq, Generic)

resignMembership
  :: ResignMembershipInputs -> Either ResignMembershipError ResignMembershipOutputs
resignMembership ResignMembershipInputs{..} = do
  TxOut address inputValue txOutDatum _ <-
    extractOutput EmptyUTxO AmbiguousUTxO scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let oldMembership = membershipUsers inputDatum
  validate ResigneeNotInMembershipGroup $ resignee `elem` oldMembership
  let newMembership = filter (/= resignee) oldMembership
  validate EmptyMembership $ not $ null newMembership
  let outputDatum = inputDatum{membershipUsers = newMembership}
  let redeemer = ResignMembership resignee
  let outputValue = txOutValueToValue inputValue
  pure ResignMembershipOutputs{..}
