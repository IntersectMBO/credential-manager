module CredentialManager.Orchestrator.InitHotNFT where

import Cardano.Api (
  Address,
  NetworkId,
  PaymentCredential (..),
  PlutusScriptV3,
  PolicyId,
  Script,
  ScriptHash,
  SerialiseAsRawBytes (..),
  ShelleyAddr,
  StakeAddressReference,
  makeShelleyAddress,
 )
import Cardano.Api.Shelley (hashScript)
import CredentialManager.Api (CertificateHash, HotLockDatum (..), Identity (..))
import CredentialManager.Orchestrator.Common (serialiseScript, validateGroup)
import CredentialManager.Scripts (hotNFT)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  Credential (..),
  CurrencySymbol (..),
  HotCommitteeCredential (..),
  PubKeyHash,
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data InitHotNFTInputs = InitHotNFTInputs
  { networkId :: NetworkId
  , coldNFTPolicyId :: PolicyId
  , hotCredentialScript :: Script PlutusScriptV3
  , stakeAddress :: StakeAddressReference
  , votingUsers :: [Identity]
  }
  deriving (Show, Eq, Generic)

data InitHotNFTOutputs = InitHotNFTOutputs
  { script :: Script PlutusScriptV3
  , scriptHash :: ScriptHash
  , scriptAddress :: Address ShelleyAddr
  , initialDatum :: HotLockDatum
  }
  deriving (Generic)

data InitHotNFTError
  = EmptyVoting
  | DuplicateVotingCertificates CertificateHash
  | DuplicateVotingPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

initHotNFT :: InitHotNFTInputs -> Either InitHotNFTError InitHotNFTOutputs
initHotNFT InitHotNFTInputs{..} = do
  validateGroup
    EmptyVoting
    DuplicateVotingCertificates
    DuplicateVotingPubKeyHash
    votingUsers
  let coldPolicy =
        CurrencySymbol $
          toBuiltin $
            serialiseToRawBytes coldNFTPolicyId
  let script =
        serialiseScript
          . hotNFT coldPolicy
          . HotCommitteeCredential
          . ScriptCredential
          . PV3.ScriptHash
          . toBuiltin
          . serialiseToRawBytes
          . hashScript
          $ hotCredentialScript
  let scriptHash = hashScript script
  let paymentCredential = PaymentCredentialByScript scriptHash
  let scriptAddress = makeShelleyAddress networkId paymentCredential stakeAddress
  let initialDatum = HotLockDatum{..}
  pure InitHotNFTOutputs{..}
