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
mintingScript validateDatum redeemer ScriptContext{scriptContextPurpose, scriptContextTxInfo} = case scriptContextPurpose of
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
    TxInfo{txInfoMint} = scriptContextTxInfo

{-# INLINEABLE validateColdLockDatum #-}
validateColdLockDatum :: BuiltinData -> Bool
validateColdLockDatum datum = case fromBuiltinData datum of
  Nothing -> traceError "Invalid cold lock datum"
  Just ColdLockDatum{delegationUsers, membershipUsers} ->
    traceIfFalse "Membership group invalid" (checkGroup membershipUsers)
      && traceIfFalse "Delegation group invalid" (checkGroup delegationUsers)

{-# INLINEABLE validateHotLockDatum #-}
validateHotLockDatum :: BuiltinData -> Bool
validateHotLockDatum datum = case fromBuiltinData datum of
  Nothing -> traceError "Invalid hot lock datum"
  Just HotLockDatum{votingUsers} ->
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
burnScript input symbol mintedTokens TxInfo{txInfoInputs} =
  checkInputs txInfoInputs && checkBurnAllListed
  where
    checkInputs [] = trace "Burn input not found" False
    checkInputs (TxInInfo ref TxOut{txOutValue} : inputs)
      | ref == input =
          traceIfFalse "Incorrect tokens burned" $ checkAllListedInValue txOutValue
      | otherwise = checkInputs inputs

    checkBurnAllListed =
      -- Check 1: The mint value only burns tokens.
      traceIfFalse
        "Mint value is not exclusively burning"
        (AMap.all (== -1) mintedTokens)

    checkAllListedInValue :: Value -> Bool
    checkAllListedInValue (Value value) = case AMap.lookup symbol value of
      Nothing -> trace "Specified input does not contain burned policy ID" False
      Just tokens ->
        -- NOTE(jamie): This check relies on the guarantee from mintScript that
        -- only one token may be minted for a given token name.
        -- Check 2: All tokens in the input value are in the mint value
        traceIfFalse
          "Mint value contains tokens not present in burn input value"
          (all (`AMap.member` mintedTokens) (AMap.keys tokens))
          -- Check 3: All tokens in the mint value are in the input value
          && traceIfFalse
            "Burn input value contains tokens not present in mint value"
            (all (`AMap.member` tokens) (AMap.keys mintedTokens))

{-# INLINEABLE mintScript #-}
mintScript
  :: Maybe (BuiltinData -> Bool)
  -> TxOutRef
  -> ScriptHash
  -> CurrencySymbol
  -> Map TokenName Integer
  -> TxInfo
  -> Bool
mintScript validateDatum seed scriptHash symbol mintedTokens TxInfo{txInfoInputs, txInfoOutputs} =
  checkMintedTokens && checkInputs txInfoInputs
  where
    checkInputs [] = traceError "Seed input not found"
    checkInputs (TxInInfo ref _ : inputs)
      | ref == seed = checkOutputs txInfoOutputs
      | otherwise = checkInputs inputs

    checkOutputs [] = traceError "Script output not found"
    checkOutputs (TxOut address value datum refScript : outputs)
      | addressMatches address =
          checkOutputValue value
            && maybe True (checkOutputDatum datum) validateDatum
            && not (any (addressMatches . txOutAddress) outputs)
            && checkOutputReferenceScript refScript
      | otherwise = checkOutputs outputs

    addressMatches :: Address -> Bool
    addressMatches Address{addressCredential} = case addressCredential of
      ScriptCredential scriptHash' -> scriptHash == scriptHash'
      _ -> False

    -- \| Refernece script should be empty
    checkOutputReferenceScript :: Maybe ScriptHash -> Bool
    checkOutputReferenceScript = traceIfFalse "Output reference script is not empty" . isNothing

    checkOutputValue :: Value -> Bool
    checkOutputValue value =
      traceIfFalse
        "Script output does not contain NFT"
        -- NOTE(jamie): If this check ever changes to allow more than one token
        -- to be minted, checkBurn in burnScript needs to be changed because it
        -- relies on this guarantee right now.
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
