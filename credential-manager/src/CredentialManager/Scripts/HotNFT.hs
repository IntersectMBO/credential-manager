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
  Identity,
 )
import CredentialManager.Scripts.Common
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValueOf)
import PlutusLedgerApi.V3 (
  Datum (..),
  FromData (..),
  GovernanceActionId,
  Map,
  OutputDatum (..),
  PubKeyHash,
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

-- | This script validates voting group actions as well as its rotation through delagators action.
{-# INLINEABLE hotNFTScript #-}
hotNFTScript
  :: AssetClass
  -> AssetClass
  -> HotCommitteeCredential
  -> ScriptContext
  -> Bool
hotNFTScript coldNFT hotNFT hotCred =
  checkSpendingTx \TxInfo{..} _ inAddress inValue datumIn -> \case
    Vote ->
      checkContinuingTx inAddress inValue txInfoOutputs \datumOut ->
        traceIfFalse "Own datum not conserved" (datumIn == datumOut)
          && checkMultiSig (votingUsers datumIn) txInfoSignatories
          && checkVote
      where
        checkVote = case Map.toList txInfoVotes of
          [(voter, voterVotes)] ->
            traceIfFalse "Unexpected voter" (voter == CommitteeVoter hotCred)
              && traceIfFalse "No votes" (not $ Map.null voterVotes)
          _ -> trace "Invalid number of voters" False
    ResignVoting user ->
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkDatumModificationTx txInfoVotes datumIn
        $ checkResignation txInfoSignatories user
    RotateHot ->
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkDatumModificationTx txInfoVotes datumIn \voting voting' ->
          signedByDelegators coldNFT txInfoSignatories txInfoReferenceInputs
            && checkRotation txInfoSignatories voting voting'
    BurnHot ->
      checkTerminalTx coldNFT txInfoVotes txInfoSignatories txInfoReferenceInputs
        && checkBurn hotNFT txInfoOutputs
    UpgradeHot destination ->
      checkTerminalTx coldNFT txInfoVotes txInfoSignatories txInfoReferenceInputs
        && checkUpgrade hotNFT destination txInfoOutputs

{-# INLINEABLE checkDatumModificationTx #-}
checkDatumModificationTx
  :: Map Voter (Map GovernanceActionId PV3.Vote)
  -> HotLockDatum
  -> ([Identity] -> [Identity] -> Bool)
  -> HotLockDatum
  -> Bool
checkDatumModificationTx votes (HotLockDatum voting) checkDiff (HotLockDatum voting') =
  traceIfFalse "Tx casts votes" (Map.null votes) && checkDiff voting voting'

{-# INLINEABLE checkTerminalTx #-}
checkTerminalTx
  :: AssetClass
  -> Map Voter (Map GovernanceActionId PV3.Vote)
  -> [PubKeyHash]
  -> [TxInInfo]
  -> Bool
checkTerminalTx coldNFT votes signatories refInputs =
  signedByDelegators coldNFT signatories refInputs
    && traceIfFalse "Tx publishes certificates" (Map.null votes)

{-# INLINEABLE signedByDelegators #-}
signedByDelegators :: AssetClass -> [PubKeyHash] -> [TxInInfo] -> Bool
signedByDelegators coldNFT signatories = go
  where
    go [] = trace "Cold NFT reference input not found" False
    go (TxInInfo _ TxOut{..} : refInputs')
      | assetClassValueOf txOutValue coldNFT > 0 = case txOutDatum of
          OutputDatum datum -> case fromBuiltinData $ getDatum datum of
            Just ColdLockDatum{..} -> checkMultiSig delegationUsers signatories
            _ -> trace "Invalid cold NFT datum" False
          _ -> trace "Missing cold NFT datum" False
      | otherwise = go refInputs'
