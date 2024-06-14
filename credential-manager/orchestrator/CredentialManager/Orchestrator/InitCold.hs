module CredentialManager.Orchestrator.InitCold where

import Cardano.Api (
  Address,
  AssetName (..),
  NetworkId,
  PaymentCredential (..),
  PlutusScriptV2,
  PlutusScriptV3,
  PlutusScriptVersion (..),
  Script,
  ScriptHash,
  SerialiseAsRawBytes (..),
  ShelleyAddr,
  StakeAddressReference,
  TxIn (..),
  TxIx (..),
  makeShelleyAddress,
 )
import Cardano.Api.Shelley (hashScript)
import Control.Monad (when)
import CredentialManager.Api (
  CertificateHash,
  ColdLockDatum (..),
  Identity (..),
  MintingRedeemer (..),
 )
import CredentialManager.Orchestrator.Common (serialiseScript, validateGroup)
import CredentialManager.Scripts (coldCommittee, coldNFT)
import CredentialManager.ScriptsV2 (minting)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  CurrencySymbol (..),
  TokenName (..),
 )
import PlutusLedgerApi.V2 (TxId (TxId), TxOutRef (..))
import qualified PlutusLedgerApi.V2 as PV2
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential (..),
  Credential (..),
  PubKeyHash,
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data InitColdInputs = InitColdInputs
  { seedInput :: TxIn
  , networkId :: NetworkId
  , stakeAddress :: StakeAddressReference
  , certificateAuthority :: Identity
  , membershipUsers :: [Identity]
  , delegationUsers :: [Identity]
  }
  deriving (Show, Eq, Generic)

data InitColdOutputs = InitColdOutputs
  { mintingScript :: Script PlutusScriptV2
  , mintingScriptHash :: ScriptHash
  , mintingRedeemer :: MintingRedeemer
  , coldNFTAssetName :: AssetName
  , credentialScript :: Script PlutusScriptV3
  , credentialScriptHash :: ScriptHash
  , nftScript :: Script PlutusScriptV3
  , nftScriptHash :: ScriptHash
  , nftScriptAddress :: Address ShelleyAddr
  , nftDatum :: ColdLockDatum
  }
  deriving (Generic)

data InitColdError
  = SeedTxIxTooLarge
  | MembershipTooSmall
  | DuplicateMembershipCertificates CertificateHash
  | DuplicateMembershipPubKeyHash PubKeyHash
  | DelegationTooSmall
  | DuplicateDelegationCertificates CertificateHash
  | DuplicateDelegationPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

initCold :: InitColdInputs -> Either InitColdError InitColdOutputs
initCold InitColdInputs{..} = do
  validateGroup
    MembershipTooSmall
    DuplicateMembershipCertificates
    DuplicateMembershipPubKeyHash
    membershipUsers
  validateGroup
    DelegationTooSmall
    DuplicateDelegationCertificates
    DuplicateDelegationPubKeyHash
    delegationUsers
  let TxIn txId (TxIx txIx) = seedInput
  let txIdBytes = serialiseToRawBytes txId
  when (txIx >= 256) $ Left SeedTxIxTooLarge
  let txIxByte = fromIntegral txIx
  let tokenName = BS.drop 4 txIdBytes <> BS.pack [BS.c2w '#', txIxByte]
  let coldNFTAssetName = AssetName tokenName
  let mintingScript = serialiseScript PlutusScriptV2 minting
  let mintingScriptHash = hashScript mintingScript
  let assetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes mintingScriptHash)
          (TokenName $ toBuiltin tokenName)
  let credentialScript = serialiseScript PlutusScriptV3 . coldCommittee $ assetClass
  let credentialScriptHash = hashScript credentialScript
  let nftScript =
        serialiseScript PlutusScriptV3
          . coldNFT
          . ColdCommitteeCredential
          . ScriptCredential
          . PV3.ScriptHash
          . toBuiltin
          . serialiseToRawBytes
          $ credentialScriptHash
  let nftScriptHash = hashScript nftScript
  let paymentCredential = PaymentCredentialByScript nftScriptHash
  let nftScriptAddress = makeShelleyAddress networkId paymentCredential stakeAddress
  let nftDatum = ColdLockDatum{..}
  let txId' = TxId $ toBuiltin $ serialiseToRawBytes txId
  let txIx' = fromIntegral txIx
  let seedInput' = TxOutRef txId' txIx'
  let nftScriptHash' = PV2.ScriptHash $ toBuiltin $ serialiseToRawBytes nftScriptHash
  let mintingRedeemer = MintCold seedInput' nftScriptHash'
  pure InitColdOutputs{..}
