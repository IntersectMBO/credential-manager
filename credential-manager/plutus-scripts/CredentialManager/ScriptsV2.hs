{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module CredentialManager.ScriptsV2 where

import CredentialManager.Scripts.Minting (mintingScript)
import PlutusTx (CompiledCode, UnsafeFromData (..), compile)
import PlutusTx.Prelude

{-# INLINEABLE wrapTwoArgs #-}
wrapTwoArgs
  :: (UnsafeFromData a, UnsafeFromData b)
  => (a -> b -> Bool)
  -> BuiltinData
  -> BuiltinData
  -> ()
wrapTwoArgs f a ctx =
  check $ f (unsafeFromBuiltinData a) (unsafeFromBuiltinData ctx)

minting :: CompiledCode (BuiltinData -> BuiltinData -> ())
minting = $$(compile [||wrapTwoArgs mintingScript||])
