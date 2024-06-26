{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

-- | The hot committee script acts as the credential authorized by the cold
-- committee credential to vote on governance actions. All it checks is that
-- the NFT provided at compile time is spent, delegating all other logic to
-- the payment credential holding the NFT.
module CredentialManager.Scripts.HotCommittee where

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

-- | This script just checks that the hard-coded "Hot NFT" is in any spending
-- input of the transaction.
{-# INLINEABLE hotCommitteeScript #-}
hotCommitteeScript :: AssetClass -> ScriptContext -> Bool
hotCommitteeScript nft ScriptContext{..} = case scriptContextScriptInfo of
  VotingScript _ ->
    traceIfFalse "Hot NFT not found in any input" $ any inputSpendsToken txInputs
  _ -> trace "Invalid script purpose" False
  where
    -- Checks if an input spends the correct token
    inputSpendsToken TxInInfo{txInInfoResolved = TxOut{..}} =
      assetClassValueOf txOutValue nft == 1
    -- The list of transaction inputs being consumed in this transaction.
    txInputs = txInfoInputs scriptContextTxInfo
