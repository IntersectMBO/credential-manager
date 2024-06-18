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

-- | The cold NFT script is used to control spending of the cold NFT. It
-- controls the business logic of the cold credential, including checking
-- allowed actions and required signers.
module CredentialManager.Scripts.ColdNFT where

import CredentialManager.Api (
  ColdLockDatum (..),
  ColdLockRedeemer (..),
 )
import CredentialManager.Scripts.Common
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  PubKeyHash,
  ScriptPurpose (..),
  TxCert (..),
  TxInInfo (..),
  TxOut (..),
 )
import PlutusLedgerApi.V3.Contexts (ColdCommitteeCredential)
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Lift as PlutusTx
import PlutusTx.Prelude hiding (traceIfFalse)
import qualified Prelude as Haskell

-- | A version of PlutusLedgerApi.V3.ScriptContext that only decodes what the
-- cold NFT script needs.
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

-- | A version of PlutusLedgerApi.V3.TxInfo that only decodes what the
-- cold NFT script needs.
data TxInfo = TxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: BuiltinData
  , txInfoOutputs :: [TxOut]
  , txInfoFee :: BuiltinData
  , txInfoMint :: BuiltinData
  , txInfoTxCerts :: [TxCert]
  , txInfoWdrl :: BuiltinData
  , txInfoValidRange :: BuiltinData
  , txInfoSignatories :: [PubKeyHash]
  , txInfoRedeemers :: BuiltinData
  , txInfoData :: BuiltinData
  , txInfoId :: BuiltinData
  , txInfoVotes :: BuiltinData
  , txInfoProposalProcedures :: BuiltinData
  , txInfoCurrentTreasuryAmount :: BuiltinData
  , txInfoTreasuryDonation :: BuiltinData
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

-- | This script validates:
-- * delegators group actions beside voting group management
-- * members group actions
{-# INLINEABLE coldNFTScript #-}
coldNFTScript
  :: AssetClass
  -> ColdCommitteeCredential
  -> ColdLockDatum
  -> ColdLockRedeemer
  -> ScriptContext
  -> Bool
coldNFTScript coldNFT coldCred datumIn@ColdLockDatum{..} red ctx =
  checkSpendingTx purpose inputs \_ ownInput -> case red of
    AuthorizeHot hotCred ->
      checkContinuingTx ownInput outputs \datumOut ->
        traceIfFalse "Own datum not conserved" (datumIn == datumOut)
          && checkMultiSig delegationUsers signatures
          && traceIfFalse "Unexpected certificates" checkAuthHotCert
      where
        checkAuthHotCert =
          txInfoTxCerts txInfo == [TxCertAuthHotCommittee coldCred hotCred]
    ResignDelegation user ->
      checkContinuingTx ownInput outputs \case
        ColdLockDatum ca' membership' delegation' ->
          traceIfFalse "CA not conserved" (certificateAuthority == ca')
            && traceIfFalse "Membership not conserved" (membershipUsers == membership')
            && traceIfFalse "Tx publishes certificates" checkNoCerts
            && checkResignation signatures user delegationUsers delegation'
    ResignMembership user ->
      checkContinuingTx ownInput outputs \case
        ColdLockDatum ca' membership' delegation' ->
          traceIfFalse "CA not conserved" (certificateAuthority == ca')
            && traceIfFalse "Delegation not conserved" (delegationUsers == delegation')
            && traceIfFalse "Tx publishes certificates" checkNoCerts
            && checkResignation signatures user membershipUsers membership'
    ResignCold ->
      checkContinuingTx ownInput outputs \datumOut ->
        traceIfFalse "own datum not conserved" (datumIn == datumOut)
          && checkMultiSig membershipUsers signatures
          && traceIfFalse "Unexpected certificates" checkAuthHotCert
      where
        checkAuthHotCert =
          txInfoTxCerts txInfo == [TxCertResignColdCommittee coldCred]
    RotateCold ->
      checkContinuingTx ownInput outputs \case
        ColdLockDatum ca' membership' delegation' ->
          traceIfFalse "CA not conserved" (certificateAuthority == ca')
            && traceIfFalse "Tx publishes certificates" checkNoCerts
            && checkMultiSig membershipUsers signatures
            && checkRotation signatures membershipUsers membership'
            && checkRotation signatures delegationUsers delegation'
    BurnCold ->
      checkMultiSig membershipUsers signatures
        && checkBurn coldNFT outputs
        && traceIfFalse "Tx publishes certificates" checkNoCerts
    UpgradeCold destination ->
      checkMultiSig membershipUsers signatures
        && checkUpgrade coldNFT destination outputs
        && traceIfFalse "Tx publishes certificates" checkNoCerts
  where
    checkNoCerts = null $ txInfoTxCerts txInfo
    purpose = scriptContextPurpose ctx
    txInfo = scriptContextTxInfo ctx
    signatures = txInfoSignatories txInfo
    outputs = txInfoOutputs txInfo
    inputs = txInfoInputs txInfo

PlutusTx.makeLift ''TxInfo
PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

PlutusTx.makeLift ''ScriptContext
PlutusTx.makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]
