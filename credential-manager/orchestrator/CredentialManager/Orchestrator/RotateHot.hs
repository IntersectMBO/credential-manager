module CredentialManager.Orchestrator.RotateHot where

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
  validateGroup,
 )
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (PubKeyHash)

data RotateHotInputs = RotateHotInputs
  { newVoting :: [Identity]
  , scriptUtxo :: UTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data RotateHotOutputs = RotateHotOutputs
  { redeemer :: HotLockRedeemer
  , outputDatum :: HotLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  }
  deriving (Show, Eq, Generic)

data RotateHotError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  | EmptyVoting
  | DuplicateVotingCertificates CertificateHash
  | DuplicateVotingPubKeyHash PubKeyHash
  | EmptyUTxO
  | AmbiguousUTxO
  deriving (Show, Eq, Generic)

rotateHot :: RotateHotInputs -> Either RotateHotError RotateHotOutputs
rotateHot RotateHotInputs{..} = do
  validateGroup
    EmptyVoting
    DuplicateVotingCertificates
    DuplicateVotingPubKeyHash
    newVoting
  TxOut address inputValue txOutDatum _ <-
    extractOutput EmptyUTxO AmbiguousUTxO scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let outputDatum = inputDatum{votingUsers = newVoting}
  let redeemer = RotateHot
  let outputValue = txOutValueToValue inputValue
  pure RotateHotOutputs{..}
