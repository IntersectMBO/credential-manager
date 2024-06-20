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
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  ScriptContext,
  TxCert (..),
  TxInfo (..),
 )
import PlutusLedgerApi.V3.Contexts (ColdCommitteeCredential)
import PlutusTx.Prelude hiding (traceIfFalse)

-- | This script validates:
-- * delegators group actions beside voting group management
-- * members group actions
{-# INLINEABLE coldNFTScript #-}
coldNFTScript
  :: AssetClass
  -> ColdCommitteeCredential
  -> ScriptContext
  -> Bool
coldNFTScript coldNFT coldCred =
  checkSpendingTx \TxInfo{..} _ inAddress inValue datumIn -> \case
    AuthorizeHot hotCred ->
      checkContinuingTx inAddress inValue txInfoOutputs \datumOut ->
        traceIfFalse "Own datum not conserved" (datumIn == datumOut)
          && checkMultiSig (delegationUsers datumIn) txInfoSignatories
          && traceIfFalse "Unexpected certificates" (txInfoTxCerts == [expectedCert])
      where
        expectedCert = TxCertAuthHotCommittee coldCred hotCred
    ResignDelegation user ->
      checkContinuingTx inAddress inValue txInfoOutputs \datumOut ->
        traceIfFalse
          "CA not conserved"
          (certificateAuthority datumIn == certificateAuthority datumOut)
          && traceIfFalse "Tx publishes certificates" (null txInfoTxCerts)
          && traceIfFalse
            "Membership not conserved"
            (membershipUsers datumIn == membershipUsers datumOut)
          && checkResignation txInfoSignatories user delegationUsers datumIn datumOut
    ResignMembership user ->
      checkContinuingTx inAddress inValue txInfoOutputs \datumOut ->
        traceIfFalse
          "CA not conserved"
          (certificateAuthority datumIn == certificateAuthority datumOut)
          && traceIfFalse "Tx publishes certificates" (null txInfoTxCerts)
          && traceIfFalse
            "Delegation not conserved"
            (delegationUsers datumIn == delegationUsers datumOut)
          && checkResignation txInfoSignatories user membershipUsers datumIn datumOut
    ResignCold ->
      checkContinuingTx inAddress inValue txInfoOutputs \datumOut ->
        traceIfFalse "Own datum not conserved" (datumIn == datumOut)
          && checkMultiSig (membershipUsers datumIn) txInfoSignatories
          && traceIfFalse "Unexpected certificates" (txInfoTxCerts == [expectedCert])
      where
        expectedCert = TxCertResignColdCommittee coldCred
    RotateCold ->
      checkContinuingTx inAddress inValue txInfoOutputs \datumOut ->
        traceIfFalse
          "CA not conserved"
          (certificateAuthority datumIn == certificateAuthority datumOut)
          && traceIfFalse "Tx publishes certificates" (null txInfoTxCerts)
          && checkMultiSig (membershipUsers datumIn) txInfoSignatories
          && checkRotation txInfoSignatories membershipUsers datumIn datumOut
          && checkRotation txInfoSignatories delegationUsers datumIn datumOut
    BurnCold ->
      checkMultiSig (membershipUsers datumIn) txInfoSignatories
        && traceIfFalse "Tx publishes certificates" (null txInfoTxCerts)
        && checkBurn coldNFT txInfoOutputs
    UpgradeCold destination ->
      checkMultiSig (membershipUsers datumIn) txInfoSignatories
        && traceIfFalse "Tx publishes certificates" (null txInfoTxCerts)
        && checkUpgrade coldNFT destination txInfoOutputs
