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
import PlutusLedgerApi.V1.Value (
  AssetClass,
  adaSymbol,
  adaToken,
  assetClassValueOf,
  flattenValue,
 )
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
  UnsafeFromData,
  Value,
  fromBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusLedgerApi.V3.Contexts (findOwnInput)
import PlutusTx.Prelude hiding (trace, traceIfFalse)
#ifdef TRACE_GHC
import qualified Prelude as H
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
checkMultiSig authorizedGroup signatories =
  traceIfFalse "insufficient signatures" $ majority <= numberOfSignatures
  where
    allSignatures = nub $ pubKeyHash <$> authorizedGroup
    majority = (\x -> divide x 2 + modulo x 2) $ length allSignatures
    numberOfSignatures = length $ filter (`elem` signatories) allSignatures

{-# INLINEABLE findOutputsByCredential #-}
findOutputsByCredential :: Credential -> [TxOut] -> [TxOut]
findOutputsByCredential ownCredential =
  filter \(TxOut (Address outCredential _) _ _ _) ->
    ownCredential == outCredential

{-# INLINEABLE checkSpendingTx #-}
checkSpendingTx
  :: (FromData datum, FromData redeemer)
  => AssetClass
  -> (TxInfo -> Address -> Value -> datum -> redeemer -> Bool)
  -> ScriptContext
  -> Bool
checkSpendingTx
  nft
  checkSpend
  scriptContext@ScriptContext
    { scriptContextScriptInfo
    , scriptContextRedeemer
    , scriptContextTxInfo
    } =
    case scriptContextScriptInfo of
      SpendingScript _ Nothing -> trace "No input datum" False
      SpendingScript _ (Just inDatum) -> case findOwnInput scriptContext of
        Nothing -> trace "Input from script purpose not found" False
        Just (TxInInfo _ TxOut{txOutAddress, txOutValue}) -> case fromBuiltinData $ getDatum inDatum of
          Nothing -> trace "Invalid input datum" False
          Just datum -> case fromBuiltinData $ getRedeemer scriptContextRedeemer of
            Nothing -> trace "Invalid redeemer" False
            Just redeemer ->
              traceIfFalse "NFT not found in input" (assetClassValueOf txOutValue nft == 1)
                && checkSpend
                  scriptContextTxInfo
                  txOutAddress
                  txOutValue
                  datum
                  redeemer
      _ -> trace "Not a spending script" False

{-# INLINEABLE checkSelfPreservation #-}
checkSelfPreservation
  :: (FromData a, Eq a)
  => Address
  -> Value
  -> [TxOut]
  -> a
  -> Bool
checkSelfPreservation addrIn valueIn outputs datumIn =
  checkContinuingTx addrIn valueIn outputs \datumOut ->
    traceIfFalse "Own datum not conserved" (datumIn == datumOut)

{-# INLINEABLE checkContinuingTx #-}
checkContinuingTx
  :: (FromData a)
  => Address
  -> Value
  -> [TxOut]
  -> (a -> Bool)
  -> Bool
checkContinuingTx addrIn valueIn outputs checkDatum =
  case findOutputsByCredential (addressCredential addrIn) outputs of
    [TxOut addrOut valueOut datumOut _] ->
      traceIfFalse "own address not preserved" (addrIn == addrOut)
        && traceIfFalse
          "own value not preserved"
          ( case flattenValue $ valueOut <> inv valueIn of
              -- Values are equal, valid
              [] -> True
              -- Values only differ in ADA and the output has more ADA than the
              -- input, valid.
              [(sym, tok, q)] -> sym == adaSymbol && tok == adaToken && q >= 0
              -- Values differ in more than just ADA, invalid.
              _ -> False
          )
        && case datumOut of
          OutputDatum (Datum datum) -> case fromBuiltinData datum of
            Just datum' -> checkDatum datum'
            Nothing -> trace "Invalid output datum" False
          OutputDatumHash _ -> trace "Inline datum required" False
          NoOutputDatum -> trace "Output has no datum" False
    [] -> trace "No continuing output found" False
    _ -> trace "Multiple continuing outputs found" False

{-# INLINEABLE checkResignation #-}
checkResignation
  :: [PubKeyHash]
  -> Identity
  -> (a -> [Identity])
  -> a
  -> a
  -> Bool
checkResignation signatories resignee getGroup datumIn datumOut =
  traceIfFalse "resignee is not a group member" checkGroupMembership
    && traceIfFalse "resignee has not signed transaction" checkSignature
    && traceIfFalse "resignee is last member of the group" checkNonEmptyOutputGroup
    && traceIfFalse "unexpected group in output" checkOutputGroup
  where
    oldGroup = getGroup datumIn
    newGroup = getGroup datumOut
    checkGroupMembership = resignee `elem` oldGroup
    checkSignature = pubKeyHash resignee `elem` signatories
    expectedNewGroup = filter (/= resignee) oldGroup
    checkNonEmptyOutputGroup = not $ null expectedNewGroup
    checkOutputGroup = newGroup == expectedNewGroup

{-# INLINEABLE checkRotation #-}
checkRotation
  :: [PubKeyHash]
  -> (a -> [Identity])
  -> a
  -> a
  -> Bool
checkRotation signatories getGroup datumIn datumOut =
  traceIfFalse "New group empty" checkNonEmptyOutputGroup
    && traceIfFalse "Added user signatures missing" checkSignatures
  where
    oldGroup = getGroup datumIn
    newGroup = getGroup datumOut
    checkNonEmptyOutputGroup = not $ null newGroup
    checkSignatures = all signedIfNew newGroup
    signedIfNew i@Identity{pubKeyHash} =
      -- NOTE(jamie) this logic is potentially confusing because it is an
      -- implication, which can be counterintuitive logical constructs. The
      -- full check is:
      --
      --    newGroupMember `implies` isSignatory
      --
      -- This expands to:
      --
      --    not newGroupMember || (newGroupMember && isSignatory)
      --
      -- which is equivalent to:
      --
      --    memberIsInOldGroup || (not memberIsInOldGroup && isSignatory)
      --
      -- However, if we are in the right branch of the or expression, we
      -- already know that `memberIsInOldGroup` must be false, so the check
      -- `not memberIsInOldGroup` is redundant. Hence, this further simplifies to:
      --
      --      memberIsInOldGroup || (not False && isSignatory)
      --    = memberIsInOldGroup || (True && isSignatory)
      --    = memberIsInOldGroup || isSignatory
      --
      -- which is precisely the check we have below.
      i `elem` oldGroup || pubKeyHash `elem` signatories

{-# INLINEABLE checkBurn #-}
checkBurn :: AssetClass -> TxInfo -> Bool
checkBurn assetClass TxInfo{txInfoMint} = assetClassValueOf txInfoMint assetClass == -1

{-# INLINEABLE checkUpgrade #-}
checkUpgrade :: AssetClass -> ScriptHash -> [TxOut] -> Bool
checkUpgrade assetClass destination = go
  where
    go [] = trace "NFT not found in outputs" False
    go (TxOut{txOutAddress, txOutValue} : outputs)
      | assetClassValueOf txOutValue assetClass == 1 =
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
