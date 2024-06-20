{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

-- | The cold committee script acts as the credential that is listed in the
-- committee state on-chain and used to authorize hot credentials. All it
-- checks is that the NFT provided at compile time is spent, delegating all
-- other logic to the payment credential holding the NFT.
module CredentialManager.Scripts.ColdCommittee where

import CredentialManager.Scripts.Common (trace, traceIfFalse)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValueOf)
import PlutusLedgerApi.V3 (
  ScriptContext (..),
  ScriptInfo (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
 )
import PlutusTx.Prelude hiding (trace, traceIfFalse)

-- | This script just checks that the hard-coded currency symbol of the NFT is
-- in any spending input of the transaction.
{-# INLINEABLE coldCommitteeScript #-}
coldCommitteeScript :: AssetClass -> ScriptContext -> Bool
coldCommitteeScript nft ScriptContext{..} = case scriptContextScriptInfo of
  CertifyingScript _ _ ->
    traceIfFalse "Cold NFT not found in any input" $ any inputSpendsToken txInputs
  _ -> trace "Invalid script purpose" False
  where
    -- Checks if an input spends the correct token
    inputSpendsToken TxInInfo{txInInfoResolved = TxOut{..}} =
      assetClassValueOf txOutValue nft == 1
    -- The list of transaction inputs being consumed in this transaction.
    txInputs = txInfoInputs scriptContextTxInfo
