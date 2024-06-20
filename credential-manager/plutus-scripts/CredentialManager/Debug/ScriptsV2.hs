{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module CredentialManager.Debug.ScriptsV2 where

import CredentialManager.Scripts.Common
import CredentialManager.Scripts.Minting (coldMintingScript, hotMintingScript)
import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude

coldMinting :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
coldMinting = $$(compile [||wrapTwoArgsV2 coldMintingScript||])

hotMinting :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
hotMinting = $$(compile [||wrapTwoArgsV2 hotMintingScript||])
