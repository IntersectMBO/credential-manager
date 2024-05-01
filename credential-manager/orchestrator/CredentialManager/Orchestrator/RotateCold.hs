module CredentialManager.Orchestrator.RotateCold where

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
  validateGroup,
 )
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (PubKeyHash)

data RotateColdInputs = RotateColdInputs
  { newMembership :: [Identity]
  , newDelegation :: [Identity]
  , scriptUtxo :: TxOut CtxUTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data RotateColdOutputs = RotateColdOutputs
  { redeemer :: ColdLockRedeemer
  , outputDatum :: ColdLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  }
  deriving (Show, Eq, Generic)

data RotateColdError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  | EmptyMembership
  | DuplicateMembershipCertificates CertificateHash
  | DuplicateMembershipPubKeyHash PubKeyHash
  | EmptyDelegation
  | DuplicateDelegationCertificates CertificateHash
  | DuplicateDelegationPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

rotateCold :: RotateColdInputs -> Either RotateColdError RotateColdOutputs
rotateCold RotateColdInputs{..} = do
  validateGroup
    EmptyMembership
    DuplicateMembershipCertificates
    DuplicateMembershipPubKeyHash
    newMembership
  validateGroup
    EmptyDelegation
    DuplicateDelegationCertificates
    DuplicateDelegationPubKeyHash
    newDelegation
  let TxOut address inputValue txOutDatum _ = scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let outputDatum =
        inputDatum
          { membershipUsers = newMembership
          , delegationUsers = newDelegation
          }
  let redeemer = RotateCold
  let outputValue = txOutValueToValue inputValue
  pure RotateColdOutputs{..}
