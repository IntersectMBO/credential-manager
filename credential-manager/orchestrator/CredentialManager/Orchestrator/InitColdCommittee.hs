{-# LANGUAGE EmptyDataDeriving #-}

module CredentialManager.Orchestrator.InitColdCommittee where

import Cardano.Api (
  AssetName,
  PlutusScriptV3,
  PolicyId,
  Script,
  ScriptHash,
  SerialiseAsRawBytes (..),
 )
import Cardano.Api.Shelley (hashScript)
import CredentialManager.Orchestrator.Common (serialiseScript)
import CredentialManager.Scripts (coldCommittee)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass (..), TokenName (..))
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  toBuiltin,
 )

data InitColdCommitteeInputs = InitColdCommitteeInputs
  { nftPolicyId :: PolicyId
  , nftAssetName :: AssetName
  }
  deriving (Show, Eq, Generic)

data InitColdCommitteeOutputs = InitColdCommitteeOutputs
  { script :: Script PlutusScriptV3
  , scriptHash :: ScriptHash
  }
  deriving (Generic)

data InitColdCommitteeError
  deriving (Show, Eq, Generic)

initColdCommittee
  :: InitColdCommitteeInputs
  -> Either InitColdCommitteeError InitColdCommitteeOutputs
initColdCommittee InitColdCommitteeInputs{..} = do
  let assetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes nftPolicyId)
          (TokenName . toBuiltin $ serialiseToRawBytes nftAssetName)
  let script = serialiseScript . coldCommittee $ assetClass
  let scriptHash = hashScript script
  pure InitColdCommitteeOutputs{..}
