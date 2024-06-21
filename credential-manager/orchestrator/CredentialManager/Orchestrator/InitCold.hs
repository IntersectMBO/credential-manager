{-# LANGUAGE RecursiveDo #-}

module CredentialManager.Orchestrator.InitCold where

import Cardano.Api (
  Address,
  AssetName (..),
  NetworkId (..),
  PaymentCredential (..),
  PlutusScriptV3,
  PlutusScriptVersion (..),
  PolicyId (..),
  Script,
  ScriptHash,
  SerialiseAsRawBytes (..),
  ShelleyAddr,
  StakeAddressReference,
  TxIn (..),
  makeShelleyAddress,
 )
import Cardano.Api.Shelley (hashScript)
import CredentialManager.Api (
  CertificateHash,
  ColdLockDatum (..),
  Identity (..),
 )
import qualified CredentialManager.Debug.Scripts as Debug
import CredentialManager.Orchestrator.Common (serialiseScript, validateGroup)
import CredentialManager.Orchestrator.InitMinting (
  DatumCheck (..),
  InitMintingInputs (..),
  InitMintingOutputs (..),
  initMinting,
 )
import qualified CredentialManager.Orchestrator.InitMinting as InitMinting
import qualified CredentialManager.Scripts as Scripts
import Data.Bifunctor (Bifunctor (..))
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  CurrencySymbol (..),
  TokenName (..),
 )
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential (..),
  Credential (..),
  PubKeyHash,
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data NFTInfo
  = NFTInfoRaw PolicyId AssetName
  | NFTInfoMint TxIn
  deriving (Show, Eq, Generic)

data InitColdInputs = InitColdInputs
  { nftInfo :: NFTInfo
  , networkId :: NetworkId
  , stakeAddress :: StakeAddressReference
  , certificateAuthority :: Identity
  , membershipUsers :: [Identity]
  , delegationUsers :: [Identity]
  , debug :: Bool
  }
  deriving (Show, Eq, Generic)

data InitColdOutputs = InitColdOutputs
  { mintingOutputs :: Maybe InitMintingOutputs
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
  | NonMintOnMainnet
  | MembershipTooSmall
  | DuplicateMembershipCertificates CertificateHash
  | DuplicateMembershipPubKeyHash PubKeyHash
  | DelegationTooSmall
  | DuplicateDelegationCertificates CertificateHash
  | DuplicateDelegationPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

initCold :: InitColdInputs -> Either InitColdError InitColdOutputs
initCold InitColdInputs{..} = mdo
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
  let destinationScript = nftScriptHash
  let datumCheck = CheckCold
  (mintingOutputs, nftPolicyId, nftTokenName) <- case nftInfo of
    NFTInfoRaw policyId tokenName -> case networkId of
      Mainnet -> Left NonMintOnMainnet
      _ -> pure (Nothing, policyId, tokenName)
    NFTInfoMint seedInput -> do
      outs@InitMintingOutputs{..} <-
        first wrapMintError $ initMinting InitMintingInputs{..}
      pure (Just outs, PolicyId mintingScriptHash, assetName)
  let coldAssetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes nftPolicyId)
          (TokenName $ toBuiltin $ serialiseToRawBytes nftTokenName)
  let credentialScript =
        serialiseScript
          PlutusScriptV3
          if debug
            then Debug.coldCommittee coldAssetClass
            else Scripts.coldCommittee coldAssetClass
  let credentialScriptHash = hashScript credentialScript
  let mkNFTScript
        | debug = Debug.coldNFT
        | otherwise = Scripts.coldNFT
  let coldCredential =
        ColdCommitteeCredential
          . ScriptCredential
          . PV3.ScriptHash
          . toBuiltin
          . serialiseToRawBytes
          $ credentialScriptHash
  let nftScript =
        serialiseScript PlutusScriptV3 $
          mkNFTScript coldAssetClass coldCredential
  let nftScriptHash = hashScript nftScript
  let paymentCredential = PaymentCredentialByScript nftScriptHash
  let nftScriptAddress = makeShelleyAddress networkId paymentCredential stakeAddress
  let nftDatum = ColdLockDatum{..}
  pure InitColdOutputs{..}

wrapMintError :: InitMinting.InitMintingError -> InitColdError
wrapMintError InitMinting.SeedTxIxTooLarge = SeedTxIxTooLarge
