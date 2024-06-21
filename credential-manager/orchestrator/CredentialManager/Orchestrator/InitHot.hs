{-# LANGUAGE RecursiveDo #-}

module CredentialManager.Orchestrator.InitHot where

import Cardano.Api (
  Address,
  AssetName (..),
  NetworkId,
  PaymentCredential (..),
  PlutusScriptV2,
  PlutusScriptV3,
  PlutusScriptVersion (..),
  PolicyId (PolicyId),
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
  HotLockDatum (..),
  Identity (..),
  MintingRedeemer (..),
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
import Data.Either (isRight)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  CurrencySymbol (..),
  TokenName (..),
 )
import PlutusLedgerApi.V3 (
  Credential (..),
  HotCommitteeCredential (..),
  PubKeyHash,
  toBuiltin,
 )
import qualified PlutusLedgerApi.V3 as PV3

data HotScriptInfo = HotScriptInfo
  { stakeAddress :: StakeAddressReference
  , coldNFTPolicyId :: PolicyId
  , coldNFTAssetName :: AssetName
  , votingUsers :: [Identity]
  }
  deriving (Show, Eq, Generic)

data InitHotInputs = InitHotInputs
  { seedInput :: TxIn
  , networkId :: NetworkId
  , scriptInfo :: Either ScriptHash HotScriptInfo
  , debug :: Bool
  }
  deriving (Show, Eq, Generic)

data HotNFTScriptOutputs = HotNFTScriptOutputs
  { nftScript :: Script PlutusScriptV3
  , nftScriptHash :: ScriptHash
  , nftScriptAddress :: Address ShelleyAddr
  , nftDatum :: HotLockDatum
  }
  deriving (Show, Eq, Generic)

data InitHotOutputs = InitHotOutputs
  { mintingScript :: Script PlutusScriptV2
  , mintingScriptHash :: ScriptHash
  , mintingRedeemer :: MintingRedeemer
  , hotNFTAssetName :: AssetName
  , credentialScript :: Script PlutusScriptV3
  , credentialScriptHash :: ScriptHash
  , nftScriptOutputs :: Maybe HotNFTScriptOutputs
  }
  deriving (Generic)

data InitHotError
  = SeedTxIxTooLarge
  | VotingTooSmall
  | DuplicateVotingCertificates CertificateHash
  | DuplicateVotingPubKeyHash PubKeyHash
  deriving (Show, Eq, Generic)

initHot :: InitHotInputs -> Either InitHotError InitHotOutputs
initHot InitHotInputs{..} = mdo
  let datumCheck = if isRight scriptInfo then CheckHot else CheckNothing
  InitMintingOutputs{..} <-
    first wrapMintError $ initMinting InitMintingInputs{..}
  let nftPolicyId = PolicyId mintingScriptHash
  let nftTokenName = assetName
  let hotAssetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes nftPolicyId)
          (TokenName $ toBuiltin $ serialiseToRawBytes nftTokenName)
  let credentialScript =
        serialiseScript
          PlutusScriptV3
          if debug
            then Debug.hotCommittee hotAssetClass
            else Scripts.hotCommittee hotAssetClass
  let credentialScriptHash = hashScript credentialScript
  (nftScriptOutputs, destinationScript) <- case scriptInfo of
    Left scriptHash -> pure (Nothing, scriptHash)
    Right HotScriptInfo{..} -> do
      validateGroup
        VotingTooSmall
        DuplicateVotingCertificates
        DuplicateVotingPubKeyHash
        votingUsers
      let coldNFTAssetClass =
            curry
              AssetClass
              (CurrencySymbol . toBuiltin $ serialiseToRawBytes coldNFTPolicyId)
              (TokenName $ toBuiltin $ serialiseToRawBytes coldNFTAssetName)
      let mkNFTScript
            | debug = Debug.hotNFT
            | otherwise = Scripts.hotNFT
      let hotCredential =
            HotCommitteeCredential
              . ScriptCredential
              . PV3.ScriptHash
              . toBuiltin
              . serialiseToRawBytes
              $ credentialScriptHash
      let nftScript =
            serialiseScript PlutusScriptV3 $
              mkNFTScript coldNFTAssetClass hotAssetClass hotCredential
      let nftScriptHash = hashScript nftScript
      let paymentCredential = PaymentCredentialByScript nftScriptHash
      let nftScriptAddress = makeShelleyAddress networkId paymentCredential stakeAddress
      let nftDatum = HotLockDatum{..}
      pure (Just HotNFTScriptOutputs{..}, nftScriptHash)
  let hotNFTAssetName = assetName
  pure InitHotOutputs{..}

wrapMintError :: InitMinting.InitMintingError -> InitHotError
wrapMintError InitMinting.SeedTxIxTooLarge = SeedTxIxTooLarge
