{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module CredentialManager.Debug.Scripts where

import CredentialManager.Scripts (wrapFiveArgs, wrapSixArgs, wrapThreeArgs)
import CredentialManager.Scripts.ColdCommittee (coldCommitteeScript)
import CredentialManager.Scripts.ColdNFT (coldNFTScript)
import CredentialManager.Scripts.HotCommittee (hotCommitteeScript)
import CredentialManager.Scripts.HotNFT (hotNFTScript)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential,
  HotCommitteeCredential,
 )
import PlutusTx (
  CompiledCode,
  ToData (..),
  compile,
  liftCodeDef,
  unsafeApplyCode,
 )
import PlutusTx.Prelude

coldCommittee
  :: AssetClass -> CompiledCode (BuiltinData -> BuiltinData -> ())
coldCommittee =
  unsafeApplyCode $$(compile [||wrapThreeArgs coldCommitteeScript||])
    . liftCodeDef
    . toBuiltinData

hotCommittee
  :: AssetClass -> CompiledCode (BuiltinData -> BuiltinData -> ())
hotCommittee =
  unsafeApplyCode $$(compile [||wrapThreeArgs hotCommitteeScript||])
    . liftCodeDef
    . toBuiltinData

coldNFT
  :: AssetClass
  -> ColdCommitteeCredential
  -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
coldNFT coldAssetClass coldCred =
  unsafeApplyCode
    ( unsafeApplyCode
        $$(compile [||wrapFiveArgs coldNFTScript||])
        (liftCodeDef (toBuiltinData coldAssetClass))
    )
    (liftCodeDef (toBuiltinData coldCred))

hotNFT
  :: AssetClass
  -> AssetClass
  -> HotCommitteeCredential
  -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
hotNFT coldAssetClass hotAssetClass hotCred =
  unsafeApplyCode
    ( unsafeApplyCode
        ( unsafeApplyCode
            $$(compile [||wrapSixArgs hotNFTScript||])
            (liftCodeDef (toBuiltinData coldAssetClass))
        )
        (liftCodeDef (toBuiltinData hotAssetClass))
    )
    (liftCodeDef (toBuiltinData hotCred))
