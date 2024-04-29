{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CredentialManager.Scripts where

import CredentialManager.Scripts.ColdCommittee (coldCommitteeScript)
import CredentialManager.Scripts.ColdNFT (coldNFTScript)
import CredentialManager.Scripts.HotCommittee (hotCommitteeScript)
import CredentialManager.Scripts.HotNFT (hotNFTScript)
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential,
  CurrencySymbol,
  HotCommitteeCredential,
 )
import PlutusTx (
  CompiledCode,
  ToData (..),
  UnsafeFromData (..),
  compile,
  liftCodeDef,
  unsafeApplyCode,
 )
import PlutusTx.Prelude

{-# INLINEABLE wrapThreeArgs #-}
wrapThreeArgs
  :: ( UnsafeFromData a
     , UnsafeFromData b
     , UnsafeFromData c
     )
  => (a -> b -> c -> Bool)
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> ()
wrapThreeArgs f a b ctx =
  check
    $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapFourArgs #-}
wrapFourArgs
  :: ( UnsafeFromData a
     , UnsafeFromData b
     , UnsafeFromData c
     , UnsafeFromData d
     )
  => (a -> b -> c -> d -> Bool)
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> ()
wrapFourArgs f a b c ctx =
  check
    $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData c)
      (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapFiveArgs #-}
wrapFiveArgs
  :: ( UnsafeFromData a
     , UnsafeFromData b
     , UnsafeFromData c
     , UnsafeFromData d
     , UnsafeFromData e
     )
  => (a -> b -> c -> d -> e -> Bool)
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> ()
wrapFiveArgs f a b c d ctx =
  check
    $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData c)
      (unsafeFromBuiltinData d)
      (unsafeFromBuiltinData ctx)

coldCommittee
  :: CurrencySymbol -> CompiledCode (BuiltinData -> BuiltinData -> ())
coldCommittee =
  unsafeApplyCode $$(compile [||wrapThreeArgs coldCommitteeScript||])
    . liftCodeDef
    . toBuiltinData

hotCommittee
  :: CurrencySymbol -> CompiledCode (BuiltinData -> BuiltinData -> ())
hotCommittee =
  unsafeApplyCode $$(compile [||wrapThreeArgs hotCommitteeScript||])
    . liftCodeDef
    . toBuiltinData

coldNFT
  :: ColdCommitteeCredential
  -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
coldNFT =
  unsafeApplyCode $$(compile [||wrapFourArgs coldNFTScript||])
    . liftCodeDef
    . toBuiltinData

hotNFT
  :: CurrencySymbol
  -> HotCommitteeCredential
  -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
hotNFT coldPolicyId hotCred =
  unsafeApplyCode
    ( unsafeApplyCode
        $$(compile [||wrapFiveArgs hotNFTScript||])
        (liftCodeDef (toBuiltinData coldPolicyId))
    )
    (liftCodeDef (toBuiltinData hotCred))
