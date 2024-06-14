module CredentialManager.Orchestrator.InitHot where

import Cardano.Api (
  Address,
  AssetName (..),
  NetworkId,
  PaymentCredential (..),
  PlutusScriptV2,
  PlutusScriptV3,
  PlutusScriptVersion (..),
  PolicyId,
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
  HotLockDatum (..),
  Identity (..),
  MintingRedeemer (..),
 )
import CredentialManager.Orchestrator.Common (serialiseScript, validateGroup)
import CredentialManager.Scripts (hotCommittee, hotNFT)
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
  Credential (..),
  HotCommitteeCredential (..),
  PubKeyHash,
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data InitHotInputs = InitHotInputs
  { seedInput :: TxIn
  , networkId :: NetworkId
  , coldNFTPolicyId :: PolicyId
  , coldNFTAssetName :: AssetName
  , stakeAddress :: StakeAddressReference
  , votingUsers :: [Identity]
  }
  deriving (Show, Eq, Generic)

data InitHotOutputs = InitHotOutputs
  { mintingScript :: Script PlutusScriptV2
  , mintingScriptHash :: ScriptHash
  , mintingRedeemer :: MintingRedeemer
  , hotNFTAssetName :: AssetName
  , credentialScript :: Script PlutusScriptV3
  , credentialScriptHash :: ScriptHash
  , nftScript :: Script PlutusScriptV3
  , nftScriptHash :: ScriptHash
  , nftScriptAddress :: Address ShelleyAddr
  , nftDatum :: HotLockDatum
  }
  deriving (Generic)

data InitHotError
  = SeedTxIxTooLarge
  | VotingTooSmall
  | DuplicateVotingCertificates CertificateHash
  | DuplicateVotingPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

initHot :: InitHotInputs -> Either InitHotError InitHotOutputs
initHot InitHotInputs{..} = do
  validateGroup
    VotingTooSmall
    DuplicateVotingCertificates
    DuplicateVotingPubKeyHash
    votingUsers
  let TxIn txId (TxIx txIx) = seedInput
  let txIdBytes = serialiseToRawBytes txId
  when (txIx >= 256) $ Left SeedTxIxTooLarge
  let txIxByte = fromIntegral txIx
  let tokenName = BS.drop 4 txIdBytes <> BS.pack [BS.c2w '#', txIxByte]
  let hotNFTAssetName = AssetName tokenName
  let mintingScript = serialiseScript PlutusScriptV2 minting
  let mintingScriptHash = hashScript mintingScript
  let coldNFTAssetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes coldNFTPolicyId)
          (TokenName $ toBuiltin $ serialiseToRawBytes coldNFTAssetName)
  let assetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes mintingScriptHash)
          (TokenName $ toBuiltin tokenName)
  let credentialScript = serialiseScript PlutusScriptV3 . hotCommittee $ assetClass
  let credentialScriptHash = hashScript credentialScript
  let nftScript =
        serialiseScript PlutusScriptV3
          . hotNFT coldNFTAssetClass
          . HotCommitteeCredential
          . ScriptCredential
          . PV3.ScriptHash
          . toBuiltin
          . serialiseToRawBytes
          $ credentialScriptHash
  let nftScriptHash = hashScript nftScript
  let paymentCredential = PaymentCredentialByScript nftScriptHash
  let nftScriptAddress = makeShelleyAddress networkId paymentCredential stakeAddress
  let nftDatum = HotLockDatum{..}
  let txId' = TxId $ toBuiltin $ serialiseToRawBytes txId
  let txIx' = fromIntegral txIx
  let seedInput' = TxOutRef txId' txIx'
  let nftScriptHash' = PV2.ScriptHash $ toBuiltin $ serialiseToRawBytes nftScriptHash
  let mintingRedeemer = MintHot seedInput' nftScriptHash'
  pure InitHotOutputs{..}