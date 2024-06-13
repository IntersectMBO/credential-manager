{-# LANGUAGE EmptyDataDeriving #-}

module CredentialManager.Orchestrator.InitHotCommittee where

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
import CredentialManager.Scripts (hotCommittee)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass (..), TokenName (..))
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  toBuiltin,
 )

data InitHotCommitteeInputs = InitHotCommitteeInputs
  { nftPolicyId :: PolicyId
  , nftAssetName :: AssetName
  }
  deriving (Show, Eq, Generic)

data InitHotCommitteeOutputs = InitHotCommitteeOutputs
  { script :: Script PlutusScriptV3
  , scriptHash :: ScriptHash
  }
  deriving (Generic)

data InitHotCommitteeError
  deriving (Show, Eq, Generic)

initHotCommittee
  :: InitHotCommitteeInputs -> Either InitHotCommitteeError InitHotCommitteeOutputs
initHotCommittee InitHotCommitteeInputs{..} = do
  let assetClass =
        curry
          AssetClass
          (CurrencySymbol . toBuiltin $ serialiseToRawBytes nftPolicyId)
          (TokenName . toBuiltin $ serialiseToRawBytes nftAssetName)
  let script = serialiseScript . hotCommittee $ assetClass
  let scriptHash = hashScript script
  pure InitHotCommitteeOutputs{..}
