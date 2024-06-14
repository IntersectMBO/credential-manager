module CredentialManager.Orchestrator.ResignVoting where

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

data ResignVotingInputs = ResignVotingInputs
  { resignee :: Identity
  , scriptUtxo :: UTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data ResignVotingOutputs = ResignVotingOutputs
  { redeemer :: HotLockRedeemer
  , outputDatum :: HotLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  }
  deriving (Show, Eq, Generic)

data ResignVotingError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  | ResigneeNotInVotingGroup
  | EmptyVoting
  | EmptyUTxO
  | AmbiguousUTxO
  deriving (Show, Eq, Generic)

resignVoting
  :: ResignVotingInputs -> Either ResignVotingError ResignVotingOutputs
resignVoting ResignVotingInputs{..} = do
  TxOut address inputValue txOutDatum _ <-
    extractOutput EmptyUTxO AmbiguousUTxO scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let oldVoting = votingUsers inputDatum
  validate ResigneeNotInVotingGroup $ resignee `elem` oldVoting
  let newVoting = filter (/= resignee) oldVoting
  validate EmptyVoting $ not $ null newVoting
  let outputDatum = inputDatum{votingUsers = newVoting}
  let redeemer = ResignVoting resignee
  let outputValue = txOutValueToValue inputValue
  pure ResignVotingOutputs{..}
