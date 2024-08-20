{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

-- | The hot NFT script is used to control spending of the hot NFT. It
-- controls the business logic of the hot credential, including checking
-- allowed actions and required signers.
module CredentialManager.Scripts.HotNFT where

import CredentialManager.Api (
  ColdLockDatum (..),
  HotLockDatum (..),
  HotLockRedeemer (..),
 )
import CredentialManager.Scripts.Common
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValueOf)
import PlutusLedgerApi.V3 (
  Datum (..),
  FromData (..),
  GovernanceActionId,
  Map,
  OutputDatum (..),
  ScriptContext,
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  Voter (..),
 )
import qualified PlutusLedgerApi.V3 as PV3
import PlutusLedgerApi.V3.Contexts (HotCommitteeCredential)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude hiding (trace, traceIfFalse)

-- | This script validates voting group actions as well as its rotation through delegators action.
{-# INLINEABLE hotNFTScript #-}
hotNFTScript
  :: AssetClass
  -> AssetClass
  -> HotCommitteeCredential
  -> ScriptContext
  -> Bool
hotNFTScript coldNFT hotNFT hotCred =
  checkSpendingTx hotNFT \txInfo@TxInfo{txInfoOutputs, txInfoReferenceInputs, txInfoSignatories, txInfoVotes} inAddress inValue datumIn -> \case
    Vote ->
      checkSelfPreservation inAddress inValue txInfoOutputs datumIn
        && checkMultiSig (votingUsers datumIn) txInfoSignatories
        && checkVote
      where
        checkVote = case Map.toList txInfoVotes of
          [(voter, voterVotes)] ->
            traceIfFalse "Unexpected voter" (voter == CommitteeVoter hotCred)
              && traceIfFalse "No votes" (not $ Map.null voterVotes)
          _ -> trace "Invalid number of voters" False
    ResignVoting user ->
      checkNoVotes txInfoVotes
        && checkContinuingTx
          inAddress
          inValue
          txInfoOutputs
          (checkResignation txInfoSignatories user votingUsers datumIn)
    RotateHot ->
      checkNoVotes txInfoVotes
        && signedByDelegators txInfoSignatories txInfoReferenceInputs
        && checkContinuingTx
          inAddress
          inValue
          txInfoOutputs
          (checkRotation txInfoSignatories votingUsers datumIn)
    BurnHot ->
      signedByDelegators txInfoSignatories txInfoReferenceInputs
        && checkNoVotes txInfoVotes
        && checkBurn hotNFT txInfo
    UpgradeHot destination ->
      signedByDelegators txInfoSignatories txInfoReferenceInputs
        && checkNoVotes txInfoVotes
        && checkUpgrade hotNFT destination txInfoOutputs
  where
    signedByDelegators signatories = go
      where
        go [] = trace "Cold NFT reference input not found" False
        go (TxInInfo _ TxOut{txOutDatum, txOutValue} : refInputs')
          | assetClassValueOf txOutValue coldNFT == 1 = case txOutDatum of
              OutputDatum datum -> case fromBuiltinData $ getDatum datum of
                Just ColdLockDatum{delegationUsers} -> checkMultiSig delegationUsers signatories
                _ -> trace "Invalid cold NFT datum" False
              _ -> trace "Missing cold NFT datum" False
          | otherwise = go refInputs'

{-# INLINEABLE checkNoVotes #-}
checkNoVotes :: Map Voter (Map GovernanceActionId PV3.Vote) -> Bool
checkNoVotes = traceIfFalse "Tx casts votes" . Map.null
