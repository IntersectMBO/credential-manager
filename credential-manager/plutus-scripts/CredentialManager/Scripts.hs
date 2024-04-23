{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CredentialManager.Scripts where

import CredentialManager.Scripts.ColdCommittee (coldCommitteeScript)
import PlutusLedgerApi.V3 (CurrencySymbol)
import PlutusTx (
  CompiledCode,
  ToData (..),
  UnsafeFromData (..),
  compile,
  liftCodeDef,
  unsafeApplyCode,
 )
import PlutusTx.Prelude

-- | Helper function to wrap a script to error on the return of a False.
{-# INLINEABLE wrapTwoArgs #-}
wrapTwoArgs
  :: (UnsafeFromData a, UnsafeFromData b)
  => (a -> b -> Bool)
  -> BuiltinData
  -> BuiltinData
  -> ()
wrapTwoArgs f a ctx =
  check
    $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)

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

coldCommittee
  :: CurrencySymbol -> CompiledCode (BuiltinData -> BuiltinData -> ())
coldCommittee =
  unsafeApplyCode $$(compile [||wrapThreeArgs coldCommitteeScript||])
    . liftCodeDef
    . toBuiltinData
