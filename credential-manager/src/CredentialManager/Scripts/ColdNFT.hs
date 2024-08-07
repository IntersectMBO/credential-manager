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
  checkSpendingTx coldNFT \txInfo@TxInfo{txInfoTxCerts, txInfoOutputs, txInfoSignatories} inAddress inValue inRefScript datumIn -> \case
    AuthorizeHot hotCred ->
      checkSelfPreservation inAddress inValue inRefScript txInfoOutputs datumIn
        && checkMultiSig (delegationUsers datumIn) txInfoSignatories
        && checkCertificate txInfoTxCerts (TxCertAuthHotCommittee coldCred hotCred)
    ResignCold ->
      checkSelfPreservation inAddress inValue inRefScript txInfoOutputs datumIn
        && checkMultiSig (membershipUsers datumIn) txInfoSignatories
        && checkCertificate txInfoTxCerts (TxCertResignColdCommittee coldCred)
    ResignDelegation user ->
      checkNoCertificates txInfoTxCerts
        && checkContinuingTx inAddress inValue inRefScript txInfoOutputs \datumOut ->
          checkCAConservation datumIn datumOut
            && checkMembershipConserved datumIn datumOut
            && checkResignation txInfoSignatories user delegationUsers datumIn datumOut
    ResignMembership user ->
      checkNoCertificates txInfoTxCerts
        && checkContinuingTx inAddress inValue inRefScript txInfoOutputs \datumOut ->
          checkCAConservation datumIn datumOut
            && checkDelegationConserved datumIn datumOut
            && checkResignation txInfoSignatories user membershipUsers datumIn datumOut
    RotateCold ->
      checkNoCertificates txInfoTxCerts
        && checkMultiSig (membershipUsers datumIn) txInfoSignatories
        && checkContinuingTx inAddress inValue inRefScript txInfoOutputs \datumOut ->
          checkCAConservation datumIn datumOut
            && checkRotation txInfoSignatories membershipUsers datumIn datumOut
            && checkRotation txInfoSignatories delegationUsers datumIn datumOut
    BurnCold ->
      checkMultiSig (membershipUsers datumIn) txInfoSignatories
        && checkNoCertificates txInfoTxCerts
        && checkBurnAll coldNFT txInfo
    UpgradeCold destination ->
      checkMultiSig (membershipUsers datumIn) txInfoSignatories
        && checkNoCertificates txInfoTxCerts
        && checkUpgrade coldNFT inAddress destination txInfoOutputs

{-# INLINEABLE checkCAConservation #-}
checkCAConservation :: ColdLockDatum -> ColdLockDatum -> Bool
checkCAConservation (ColdLockDatum ca _ _) (ColdLockDatum ca' _ _) =
  traceIfFalse "CA not conserved" (ca == ca')

{-# INLINEABLE checkMembershipConserved #-}
checkMembershipConserved :: ColdLockDatum -> ColdLockDatum -> Bool
checkMembershipConserved (ColdLockDatum _ membership _) (ColdLockDatum _ membership' _) =
  traceIfFalse "Membership group not conserved" (membership == membership')

{-# INLINEABLE checkDelegationConserved #-}
checkDelegationConserved :: ColdLockDatum -> ColdLockDatum -> Bool
checkDelegationConserved (ColdLockDatum _ _ delegation) (ColdLockDatum _ _ delegation') =
  traceIfFalse "Delegation group not conserved" (delegation == delegation')

{-# INLINEABLE checkCertificate #-}
checkCertificate :: [TxCert] -> TxCert -> Bool
checkCertificate actual expected = traceIfFalse "Unexpected certificates" $ actual == [expected]

{-# INLINEABLE checkNoCertificates #-}
checkNoCertificates :: [TxCert] -> Bool
checkNoCertificates = traceIfFalse "Tx publishes certificates" . null
