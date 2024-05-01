{-# LANGUAGE EmptyDataDeriving #-}

module CredentialManager.Orchestrator.InitColdCommittee where

import Cardano.Api (
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
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  toBuiltin,
 )

newtype InitColdCommitteeInputs = InitColdCommitteeInputs
  { nftPolicyId :: PolicyId
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
  let script =
        serialiseScript
          . coldCommittee
          . CurrencySymbol
          . toBuiltin
          $ serialiseToRawBytes nftPolicyId
  let scriptHash = hashScript script
  pure InitColdCommitteeOutputs{..}
