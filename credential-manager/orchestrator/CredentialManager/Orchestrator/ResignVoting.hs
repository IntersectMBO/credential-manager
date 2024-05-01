module CredentialManager.Orchestrator.ResignVoting where

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

data ResignVotingInputs = ResignVotingInputs
  { resignee :: Identity
  , scriptUtxo :: TxOut CtxUTxO ConwayEra
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
  deriving (Show, Eq, Generic)

resignVoting
  :: ResignVotingInputs -> Either ResignVotingError ResignVotingOutputs
resignVoting ResignVotingInputs{..} = do
  let TxOut address inputValue txOutDatum _ = scriptUtxo
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
