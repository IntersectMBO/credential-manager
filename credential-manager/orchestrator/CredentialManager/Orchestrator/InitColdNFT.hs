module CredentialManager.Orchestrator.InitColdNFT where

import Cardano.Api (
  Address,
  NetworkId,
  PaymentCredential (..),
  PlutusScriptV3,
  Script,
  ScriptHash,
  SerialiseAsRawBytes (..),
  ShelleyAddr,
  StakeAddressReference,
  makeShelleyAddress,
 )
import Cardano.Api.Shelley (hashScript)
import CredentialManager.Api (
  CertificateHash,
  ColdLockDatum (..),
  Identity (..),
 )
import CredentialManager.Orchestrator.Common (serialiseScript, validateGroup)
import CredentialManager.Scripts (coldNFT)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential (..),
  Credential (..),
  PubKeyHash,
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data InitColdNFTInputs = InitColdNFTInputs
  { networkId :: NetworkId
  , coldCredentialScript :: Script PlutusScriptV3
  , stakeAddress :: StakeAddressReference
  , certificateAuthority :: Identity
  , membershipUsers :: [Identity]
  , delegationUsers :: [Identity]
  }
  deriving (Show, Eq, Generic)

data InitColdNFTOutputs = InitColdNFTOutputs
  { script :: Script PlutusScriptV3
  , scriptHash :: ScriptHash
  , scriptAddress :: Address ShelleyAddr
  , initialDatum :: ColdLockDatum
  }
  deriving (Generic)

data InitColdNFTError
  = EmptyMembership
  | DuplicateMembershipCertificates CertificateHash
  | DuplicateMembershipPubKeyHash PubKeyHash
  | EmptyDelegation
  | DuplicateDelegationCertificates CertificateHash
  | DuplicateDelegationPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

initColdNFT :: InitColdNFTInputs -> Either InitColdNFTError InitColdNFTOutputs
initColdNFT InitColdNFTInputs{..} = do
  validateGroup
    EmptyMembership
    DuplicateMembershipCertificates
    DuplicateMembershipPubKeyHash
    membershipUsers
  validateGroup
    EmptyDelegation
    DuplicateDelegationCertificates
    DuplicateDelegationPubKeyHash
    delegationUsers
  let script =
        serialiseScript
          . coldNFT
          . ColdCommitteeCredential
          . ScriptCredential
          . PV3.ScriptHash
          . toBuiltin
          . serialiseToRawBytes
          . hashScript
          $ coldCredentialScript
  let scriptHash = hashScript script
  let paymentCredential = PaymentCredentialByScript scriptHash
  let scriptAddress = makeShelleyAddress networkId paymentCredential stakeAddress
  let initialDatum = ColdLockDatum{..}
  pure InitColdNFTOutputs{..}
