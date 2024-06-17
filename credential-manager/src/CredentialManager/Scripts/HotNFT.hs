{-# LANGUAGE TemplateHaskell #-}
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
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValueOf)
import PlutusLedgerApi.V3 (
  Datum (..),
  FromData (..),
  GovernanceActionId,
  Map,
  OutputDatum (..),
  PubKeyHash,
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  Voter (..),
 )
import qualified PlutusLedgerApi.V3 as PV3
import PlutusLedgerApi.V3.Contexts (HotCommitteeCredential)
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Lift as PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | A version of PlutusLedgerApi.V3.ScriptContext that only decodes what the
-- cold NFT script needs.
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

-- | A version of PlutusLedgerApi.V3.ScriptPurpose that only decodes what the
-- cold NFT script needs.
data ScriptPurpose
  = Minting BuiltinData
  | Spending TxOutRef
  | Rewarding BuiltinData
  | Certifying BuiltinData BuiltinData
  | Voting BuiltinData
  | Proposing BuiltinData BuiltinData
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

-- | A version of PlutusLedgerApi.V3.TxInfo that only decodes what the
-- cold NFT script needs.
data TxInfo = TxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: [TxInInfo]
  , txInfoOutputs :: [TxOut]
  , txInfoFee :: BuiltinData
  , txInfoMint :: BuiltinData
  , txInfoTxCerts :: BuiltinData
  , txInfoWdrl :: BuiltinData
  , txInfoValidRange :: BuiltinData
  , txInfoSignatories :: [PubKeyHash]
  , txInfoRedeemers :: BuiltinData
  , txInfoData :: BuiltinData
  , txInfoId :: BuiltinData
  , txInfoVotes :: Map Voter (Map GovernanceActionId PV3.Vote)
  , txInfoProposalProcedures :: BuiltinData
  , txInfoCurrentTreasuryAmount :: BuiltinData
  , txInfoTreasuryDonation :: BuiltinData
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

-- | Given a UTXO reference and a transaction (`TxInfo`), resolve it to one of the
-- transaction's inputs (`TxInInfo`).
--
-- Note: this only searches the true transaction inputs and not the referenced transaction inputs.
{-# INLINEABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo
findTxInByTxOutRef outRef TxInfo{txInfoInputs} =
  find
    (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef)
    txInfoInputs

-- | Find the reference input that contains 1 of a certain token.
{-# INLINEABLE findTxInByNFTInRefUTxO #-}
findTxInByNFTInRefUTxO :: AssetClass -> TxInfo -> Maybe TxInInfo
findTxInByNFTInRefUTxO token txInfo =
  find
    (\txIn -> assetClassValueOf (txOutValue $ txInInfoResolved txIn) token == 1)
    (txInfoReferenceInputs txInfo)

-- | This script validates voting group actions as well as its rotation through delagators action.
{-# INLINEABLE hotNFTScript #-}
hotNFTScript
  :: AssetClass
  -> AssetClass
  -> HotCommitteeCredential
  -> HotLockDatum
  -> HotLockRedeemer
  -> ScriptContext
  -> Bool
hotNFTScript coldNFT hotNFT hotCred datumIn@HotLockDatum{..} red ctx =
  case scriptContextPurpose ctx of
    Spending txOurRef -> case findTxInByTxOutRef txOurRef txInfo of
      Nothing -> trace "Own input not found" False
      Just (TxInInfo _ ownInput) -> case red of
        Vote ->
          checkContinuingTx ownInput outputs \datumOut ->
            traceIfFalse "Own datum not conserved" (datumIn == datumOut)
              && checkMultiSig votingUsers signatures
              && checkVote
          where
            checkVote = case Map.toList $ txInfoVotes txInfo of
              [(voter, voterVotes)] ->
                traceIfFalse "Unexpected voter" (voter == CommitteeVoter hotCred)
                  && traceIfFalse "No votes" (not $ Map.null voterVotes)
              _ -> trace "Invalid number of voters" False
        ResignVoting user ->
          checkContinuingTx ownInput outputs \case
            HotLockDatum voting' ->
              traceIfFalse "Tx casts votes" checkNoVotes
                && checkResignation signatures user votingUsers voting'
        RotateHot ->
          checkContinuingTx ownInput outputs \case
            HotLockDatum voting' ->
              traceIfFalse "Tx casts votes" checkNoVotes
                && signedByDelegators ()
                && checkRotation signatures votingUsers voting'
        BurnHot ->
          signedByDelegators ()
            && checkBurn hotNFT outputs
            && traceIfFalse "Tx casts votes" checkNoVotes
        UpgradeHot destination ->
          signedByDelegators ()
            && checkUpgrade hotNFT destination outputs
            && traceIfFalse "Tx casts votes" checkNoVotes
    _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    signatures = txInfoSignatories txInfo
    outputs = txInfoOutputs txInfo
    signedByDelegators _ =
      case findTxInByNFTInRefUTxO coldNFT txInfo of
        Nothing -> trace "Cold NFT reference input not found" False
        Just (TxInInfo _ refInput) -> case txOutDatum refInput of
          OutputDatum datum -> case fromBuiltinData $ getDatum datum of
            Just ColdLockDatum{..} ->
              checkMultiSig delegationUsers signatures
            _ -> trace "Invalid cold NFT datum" False
          _ -> trace "Missing cold NFT datum" False
    checkNoVotes = Map.null $ txInfoVotes txInfo

PlutusTx.makeLift ''ScriptPurpose
PlutusTx.makeIsDataIndexed
  ''ScriptPurpose
  [ ('Minting, 0)
  , ('Spending, 1)
  , ('Rewarding, 2)
  , ('Certifying, 3)
  , ('Voting, 4)
  , ('Proposing, 5)
  ]

PlutusTx.makeLift ''TxInfo
PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

PlutusTx.makeLift ''ScriptContext
PlutusTx.makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]
