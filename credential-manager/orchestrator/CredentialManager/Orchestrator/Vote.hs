module CredentialManager.Orchestrator.Vote where

import Cardano.Api (
  Address,
  ConwayEra,
  CtxUTxO,
  PlutusScriptV3,
  Script,
  ShelleyAddr,
  TxOut (..),
  Value,
  txOutValueToValue,
 )
import Cardano.Api.Ledger (
  Anchor (..),
  Credential (..),
  GovActionId (..),
  StandardCrypto,
  StrictMaybe (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Api.Shelley (ShelleyLedgerEra, hashScript, toShelleyScriptHash)
import qualified Cardano.Api.Shelley as Shelley
import qualified Cardano.Ledger.Conway as L
import CredentialManager.Api
import CredentialManager.Orchestrator.Common (
  decodeDatum,
  getInlineDatum,
  getScriptAddress,
 )
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)

data VoteInputs = VoteInputs
  { hotCredentialScript :: Script PlutusScriptV3
  , scriptUtxo :: TxOut CtxUTxO ConwayEra
  , votes :: Map (GovActionId StandardCrypto) (Vote, Anchor StandardCrypto)
  }
  deriving (Show, Eq, Generic)

data VoteOutputs = VoteOutputs
  { redeemer :: HotLockRedeemer
  , outputDatum :: HotLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  , votingProcedures :: Shelley.VotingProcedures ConwayEra
  }
  deriving (Show, Eq, Generic)

data VoteError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  deriving (Show, Eq, Generic)

vote :: VoteInputs -> Either VoteError VoteOutputs
vote VoteInputs{..} = do
  let TxOut address inputValue txOutDatum _ = scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let hotCredentialScriptHash = hashScript hotCredentialScript
  let redeemer = Vote
  let outputDatum = inputDatum
  let outputValue = txOutValueToValue inputValue
  let voter =
        CommitteeVoter $
          ScriptHashObj $
            toShelleyScriptHash hotCredentialScriptHash
  let votingProcedures =
        Shelley.VotingProcedures
          . VotingProcedures @(ShelleyLedgerEra ConwayEra)
          . Map.singleton voter
          $ toVotingProcedure <$> votes
  pure VoteOutputs{..}

toVotingProcedure
  :: (Vote, Anchor StandardCrypto)
  -> VotingProcedure (L.ConwayEra StandardCrypto)
toVotingProcedure (vote_, anchor) = VotingProcedure vote_ $ SJust anchor
