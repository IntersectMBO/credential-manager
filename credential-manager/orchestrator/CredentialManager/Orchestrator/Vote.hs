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
  AnchorData,
  Credential (..),
  GovActionId (..),
  SafeHash,
  StandardCrypto,
  StrictMaybe (..),
  Url,
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  hashToBytes,
 )
import Cardano.Api.Shelley (ShelleyLedgerEra, hashScript, toShelleyScriptHash)
import qualified Cardano.Api.Shelley as Shelley
import Cardano.Ledger.Conway.Governance (GovActionIx (..))
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.TxIn (TxId (..))
import CredentialManager.Api
import CredentialManager.Orchestrator.Common (
  decodeDatum,
  getInlineDatum,
  getScriptAddress,
 )
import qualified Data.Map as Map
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  GovernanceActionId (..),
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data VoteInputs = VoteInputs
  { hotCredentialScript :: Script PlutusScriptV3
  , scriptUtxo :: TxOut CtxUTxO ConwayEra
  , govActionId :: GovActionId StandardCrypto
  , vote_ :: Vote
  , metadataUrl :: Url
  , metadataHash :: SafeHash StandardCrypto AnchorData
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
  let plutusGovActionId = case govActionId of
        GovActionId (TxId txId) (GovActionIx ix) ->
          GovernanceActionId
            (PV3.TxId $ toBuiltin $ hashToBytes $ extractHash txId)
            (fromIntegral ix)
  let plutusVote = case vote_ of
        VoteYes -> PV3.VoteYes
        VoteNo -> PV3.VoteNo
        Abstain -> PV3.Abstain
  let redeemer = Vote plutusGovActionId plutusVote
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
          . Map.singleton govActionId
          . VotingProcedure vote_
          . SJust
          $ Anchor metadataUrl metadataHash
  pure VoteOutputs{..}
