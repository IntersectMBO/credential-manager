{-# LANGUAGE EmptyDataDeriving #-}

module CredentialManager.Orchestrator.InitHotCommittee where

import Cardano.Api (
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
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  toBuiltin,
 )

newtype InitHotCommitteeInputs = InitHotCommitteeInputs
  { nftPolicyId :: PolicyId
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
  let script =
        serialiseScript
          . hotCommittee
          . CurrencySymbol
          . toBuiltin
          $ serialiseToRawBytes nftPolicyId
  let scriptHash = hashScript script
  pure InitHotCommitteeOutputs{..}
