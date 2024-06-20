{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module CredentialManager.Scripts.Common where

-- #define TRACE_GHC
import CredentialManager.Api (Identity (..))
#ifdef TRACE_GHC
import qualified Debug.Trace as D
#endif
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValueOf)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  Datum (..),
  FromData,
  OutputDatum (..),
  PubKeyHash,
  Redeemer (..),
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef,
  UnsafeFromData,
  Value,
  fromBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Prelude hiding (trace, traceIfFalse)
#ifdef TRACE_GHC
import qualified Prelude as H
import qualified PlutusLedgerApi.V2 as V2
#else
import qualified PlutusTx.Prelude as H
import qualified PlutusTx.Prelude as D
import qualified PlutusLedgerApi.V2 as V2
#endif

#ifdef TRACE_GHC
type String = H.String
#else
type String = H.BuiltinString
#endif

trace :: String -> a -> a
trace = D.trace

traceIfFalse :: String -> Bool -> Bool
traceIfFalse str False = trace str False
traceIfFalse _ True = True

{-# INLINEABLE checkMultiSig #-}
checkMultiSig :: [Identity] -> [PubKeyHash] -> Bool
checkMultiSig [] _ = trace "Empty multisig requirement" False
checkMultiSig list signatures =
  traceIfFalse "insufficient signatures" $ majority <= numberOfSignatures
  where
    allSignatures = nub $ pubKeyHash <$> list
    majority = (\x -> divide x 2 + modulo x 2) $ length allSignatures
    numberOfSignatures = length $ filter (`elem` signatures) allSignatures

{-# INLINEABLE findOutputsByCredential #-}
findOutputsByCredential :: Credential -> [TxOut] -> [TxOut]
findOutputsByCredential ownCredential =
  filter \(TxOut (Address outCredential _) _ _ _) ->
    ownCredential == outCredential

{-# INLINEABLE checkResignation #-}
checkResignation
  :: [PubKeyHash]
  -> Identity
  -> [Identity]
  -> [Identity]
  -> Bool
checkResignation signatories resignee oldGroup newGroup =
  traceIfFalse "resignee is not a group member" checkGroupMembership
    && traceIfFalse "resignee has not signed transaction" checkSignature
    && traceIfFalse "resignee is last member of the group" checkNonEmptyOutputGroup
    && traceIfFalse "unexpected group in output" checkOutputGroup
  where
    checkGroupMembership = resignee `elem` oldGroup
    checkSignature = pubKeyHash resignee `elem` signatories
    expectedNewGroup = filter (/= resignee) oldGroup
    checkNonEmptyOutputGroup = not $ null expectedNewGroup
    checkOutputGroup = newGroup == expectedNewGroup

{-# INLINEABLE checkSpendingTx #-}
checkSpendingTx
  :: (FromData datum, FromData redeemer)
  => (TxInfo -> TxOutRef -> Address -> Value -> datum -> redeemer -> Bool)
  -> ScriptContext
  -> Bool
checkSpendingTx checkSpend ScriptContext{..} =
  case scriptContextScriptInfo of
    SpendingScript _ Nothing -> trace "No input datum" False
    SpendingScript ownRef (Just inDatum) -> go txInfoInputs
      where
        TxInfo{..} = scriptContextTxInfo
        go [] = trace "Input from script purpose not found" False
        go (TxInInfo ref TxOut{..} : inputs')
          | ref == ownRef = case fromBuiltinData $ getDatum inDatum of
              Nothing -> trace "Invalid input datum" False
              Just datum -> case fromBuiltinData $ getRedeemer scriptContextRedeemer of
                Nothing -> trace "Invalid redeemer" False
                Just redeemer ->
                  checkSpend
                    scriptContextTxInfo
                    ownRef
                    txOutAddress
                    txOutValue
                    datum
                    redeemer
          | otherwise = go inputs'
    _ -> trace "Not a spending script" False

{-# INLINEABLE checkContinuingTx #-}
checkContinuingTx
  :: (FromData a) => Address -> Value -> [TxOut] -> (a -> Bool) -> Bool
checkContinuingTx addrIn valueIn outputs checkDatum =
  case findOutputsByCredential (addressCredential addrIn) outputs of
    [TxOut addrOut valueOut datumOut _] ->
      traceIfFalse "own address not preserved" (addrIn == addrOut)
        && traceIfFalse "own value not preserved" (valueIn == valueOut)
        && case datumOut of
          OutputDatum (Datum datum) -> case fromBuiltinData datum of
            Just datum' -> checkDatum datum'
            Nothing -> trace "Invalid output datum" False
          _ -> trace "Output has no datum" False
    [] -> trace "No continuing output found" False
    _ -> trace "Multiple continuing outputs found" False

{-# INLINEABLE checkRotation #-}
checkRotation
  :: [PubKeyHash]
  -> [Identity]
  -> [Identity]
  -> Bool
checkRotation signatories oldGroup newGroup =
  traceIfFalse "New group empty" checkNonEmptyOutputGroup
    && traceIfFalse "Added user signatures missing" checkSignatures
  where
    checkNonEmptyOutputGroup = not $ null newGroup
    checkSignatures = all signedIfNew newGroup
    signedIfNew i@Identity{..} =
      i `elem` oldGroup || pubKeyHash `elem` signatories

{-# INLINEABLE checkBurn #-}
checkBurn :: AssetClass -> [TxOut] -> Bool
checkBurn assetClass = traceIfFalse "NFT not burned" . not . any containsAsset
  where
    containsAsset TxOut{..} = assetClassValueOf txOutValue assetClass > 0

{-# INLINEABLE checkUpgrade #-}
checkUpgrade :: AssetClass -> ScriptHash -> [TxOut] -> Bool
checkUpgrade assetClass destination = go
  where
    go [] = trace "NFT not found in outputs" False
    go (TxOut{..} : outputs)
      | assetClassValueOf txOutValue assetClass > 0 =
          case addressCredential txOutAddress of
            ScriptCredential scriptHash ->
              traceIfFalse "NFT sent to incorrect script" $ destination == scriptHash
            _ -> trace "NFT sent to key hash address" False
      | otherwise = go outputs

{-# INLINEABLE wrapTwoArgsV2 #-}
wrapTwoArgsV2
  :: (UnsafeFromData a)
  => (a -> V2.ScriptContext -> Bool)
  -> BuiltinData
  -> BuiltinData
  -> BuiltinUnit
wrapTwoArgsV2 f a ctx =
  check
    $ f (unsafeFromBuiltinData a) (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapTwoArgs #-}
wrapTwoArgs
  :: (a -> ScriptContext -> Bool)
  -> a
  -> BuiltinData
  -> BuiltinUnit
wrapTwoArgs f a = check . f a . unsafeFromBuiltinData

{-# INLINEABLE wrapThreeArgs #-}
wrapThreeArgs
  :: (a -> b -> ScriptContext -> Bool)
  -> a
  -> b
  -> BuiltinData
  -> BuiltinUnit
wrapThreeArgs f a b = check . f a b . unsafeFromBuiltinData

{-# INLINEABLE wrapFourArgs #-}
wrapFourArgs
  :: (a -> b -> c -> ScriptContext -> Bool)
  -> a
  -> b
  -> c
  -> BuiltinData
  -> BuiltinUnit
wrapFourArgs f a b c = check . f a b c . unsafeFromBuiltinData
