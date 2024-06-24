{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module CredentialManager.Scripts.Minting where

import CredentialManager.Api (
  ColdLockDatum (..),
  HotLockDatum (..),
  Identity (..),
  MintingRedeemer (..),
 )
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2
import qualified PlutusTx.AssocMap as AMap
import PlutusTx.Prelude

-- | A version of the minting script that performs no datum checks on the
-- output.
{-# INLINEABLE genericMintingScript #-}
genericMintingScript :: MintingRedeemer -> ScriptContext -> Bool
genericMintingScript = mintingScript Nothing

-- | A version of the minting script that looks for a cold NFT lock script
-- datum in the output and validates it.
{-# INLINEABLE coldMintingScript #-}
coldMintingScript :: MintingRedeemer -> ScriptContext -> Bool
coldMintingScript = mintingScript $ Just validateColdLockDatum

-- | A version of the minting script that looks for a hot NFT lock script
-- datum in the output and validates it.
{-# INLINEABLE hotMintingScript #-}
hotMintingScript :: MintingRedeemer -> ScriptContext -> Bool
hotMintingScript = mintingScript $ Just validateHotLockDatum

-- | A minting script that can mint one NFT with a unique token name and send
-- it to a script, with an optional datum check function.
{-# INLINEABLE mintingScript #-}
mintingScript
  :: Maybe (BuiltinData -> Bool) -> MintingRedeemer -> ScriptContext -> Bool
mintingScript validateDatum redeemer ScriptContext{..} = case scriptContextPurpose of
  Minting symbol -> case AMap.lookup symbol (getValue txInfoMint) of
    Nothing -> traceError "Transaction does not mint any tokens"
    Just mintedTokens -> case redeemer of
      Burn input ->
        burnScript input symbol mintedTokens scriptContextTxInfo
      Mint seed scriptHash ->
        mintScript
          validateDatum
          seed
          scriptHash
          symbol
          mintedTokens
          scriptContextTxInfo
  _ -> traceError "Wrong script purpose"
  where
    TxInfo{..} = scriptContextTxInfo

{-# INLINEABLE validateColdLockDatum #-}
validateColdLockDatum :: BuiltinData -> Bool
validateColdLockDatum datum = case fromBuiltinData datum of
  Nothing -> traceError "Invalid cold lock datum"
  Just ColdLockDatum{..} ->
    traceIfFalse "Membership group invalid" (checkGroup membershipUsers)
      && traceIfFalse "Delegation group invalid" (checkGroup delegationUsers)

{-# INLINEABLE validateHotLockDatum #-}
validateHotLockDatum :: BuiltinData -> Bool
validateHotLockDatum datum = case fromBuiltinData datum of
  Nothing -> traceError "Invalid hot lock datum"
  Just HotLockDatum{..} ->
    traceIfFalse "Voting group invalid" $ checkGroup votingUsers

{-# INLINEABLE checkGroup #-}
checkGroup :: [Identity] -> Bool
checkGroup group =
  traceIfFalse "Group contains duplicate keys" (groupLength == length uniqueGroup)
    && traceIfFalse "Group is empty" (not $ null group)
  where
    groupLength = length group
    uniqueGroup =
      nubBy (\(Identity pkh _) (Identity pkh' _) -> pkh == pkh') group

{-# INLINEABLE burnScript #-}
burnScript
  :: TxOutRef
  -> CurrencySymbol
  -> Map TokenName Integer
  -> TxInfo
  -> Bool
burnScript input symbol mintedTokens TxInfo{..} =
  checkInputs txInfoInputs
  where
    checkInputs [] = trace "Burn input not found" False
    checkInputs (TxInInfo ref TxOut{..} : inputs)
      | ref == input =
          traceIfFalse "Incorrect tokens burned" $ checkBurn txOutValue
      | otherwise = checkInputs inputs

    checkBurn :: Value -> Bool
    checkBurn (Value value) = case AMap.lookup symbol value of
      Nothing -> trace "Specified input does not contain burned policy ID" False
      Just tokens ->
        all (uncurry isBurned) (AMap.toList tokens)
          && all (outputHasToken tokens) (AMap.keys mintedTokens)

    isBurned :: TokenName -> Integer -> Bool
    isBurned name q = case AMap.lookup name mintedTokens of
      Nothing -> trace "Token not burned" False
      Just q' -> traceIfFalse "Incorrect quantity burned" $ q == -q'

    outputHasToken :: Map TokenName Integer -> TokenName -> Bool
    outputHasToken tokens token =
      traceIfFalse "Burned token not found in input" $ AMap.member token tokens

{-# INLINEABLE mintScript #-}
mintScript
  :: Maybe (BuiltinData -> Bool)
  -> TxOutRef
  -> ScriptHash
  -> CurrencySymbol
  -> Map TokenName Integer
  -> TxInfo
  -> Bool
mintScript validateDatum seed scriptHash symbol mintedTokens TxInfo{..} =
  checkMintedTokens && checkInputs txInfoInputs
  where
    checkInputs [] = traceError "Seed input not found"
    checkInputs (TxInInfo ref _ : inputs)
      | ref == seed = checkOutputs txInfoOutputs
      | otherwise = checkInputs inputs

    checkOutputs [] = traceError "Script output not found"
    checkOutputs (TxOut address value datum _ : outputs)
      | addressMatches address =
          checkOutputValue value
            && maybe True (checkOutputDatum datum) validateDatum
            && not (any (addressMatches . txOutAddress) outputs)
      | otherwise = checkOutputs outputs

    addressMatches :: Address -> Bool
    addressMatches Address{..} = case addressCredential of
      ScriptCredential scriptHash' -> scriptHash == scriptHash'
      _ -> False

    checkOutputValue :: Value -> Bool
    checkOutputValue value =
      traceIfFalse
        "Script output does not contain NFT"
        (valueOf value symbol tokenName == 1)

    checkOutputDatum NoOutputDatum _ = traceError "Output datum missing"
    checkOutputDatum OutputDatumHash{} _ = traceError "Output datum is a hash"
    checkOutputDatum (OutputDatum (Datum datum)) f = f datum

    tokenName :: TokenName
    tokenName
      | seedIx >= 256 = traceError "Seed input index must be less than 256"
      | otherwise =
          TokenName
            . appendByteString (dropByteString 4 seedId)
            . consByteString 35 -- '#' in ascii
            $ consByteString seedIx emptyByteString

    TxOutRef (TxId seedId) seedIx = seed

    checkMintedTokens :: Bool
    checkMintedTokens =
      traceIfFalse
        "Incorrect number of tokens minted"
        (AMap.toList mintedTokens == [(tokenName, 1)])
