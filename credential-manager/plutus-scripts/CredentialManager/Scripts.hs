{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module CredentialManager.Scripts where

import CredentialManager.Scripts.ColdCommittee (coldCommitteeScript)
import CredentialManager.Scripts.ColdNFT (coldNFTScript)
import CredentialManager.Scripts.Common
import CredentialManager.Scripts.HotCommittee (hotCommitteeScript)
import CredentialManager.Scripts.HotNFT (hotNFTScript)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential,
  HotCommitteeCredential,
 )
import PlutusTx (
  CompiledCode,
  compile,
  liftCodeDef,
  unsafeApplyCode,
 )
import PlutusTx.Prelude

coldCommittee
  :: AssetClass -> CompiledCode (BuiltinData -> BuiltinUnit)
coldCommittee =
  unsafeApplyCode $$(compile [||wrapTwoArgs coldCommitteeScript||])
    . liftCodeDef

hotCommittee
  :: AssetClass -> CompiledCode (BuiltinData -> BuiltinUnit)
hotCommittee =
  unsafeApplyCode $$(compile [||wrapTwoArgs hotCommitteeScript||])
    . liftCodeDef

coldNFT
  :: AssetClass
  -> ColdCommitteeCredential
  -> CompiledCode (BuiltinData -> BuiltinUnit)
coldNFT coldAssetClass coldCred =
  unsafeApplyCode
    ( unsafeApplyCode
        $$(compile [||wrapThreeArgs coldNFTScript||])
        (liftCodeDef coldAssetClass)
    )
    (liftCodeDef coldCred)

hotNFT
  :: AssetClass
  -> AssetClass
  -> HotCommitteeCredential
  -> CompiledCode (BuiltinData -> BuiltinUnit)
hotNFT coldAssetClass hotAssetClass hotCred =
  unsafeApplyCode
    ( unsafeApplyCode
        ( unsafeApplyCode
            $$(compile [||wrapFourArgs hotNFTScript||])
            (liftCodeDef coldAssetClass)
        )
        (liftCodeDef hotAssetClass)
    )
    (liftCodeDef hotCred)
