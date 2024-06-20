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
  Identity,
 )
import CredentialManager.Scripts.Common
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  PubKeyHash,
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
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkCertifyingTx
          delegationUsers
          (TxCertAuthHotCommittee coldCred hotCred)
          txInfoTxCerts
          txInfoSignatories
          datumIn
    ResignDelegation user ->
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkDatumModificationTx
          txInfoTxCerts
          datumIn
          \membership membership' delegation delegation' ->
            traceIfFalse "Membership not conserved" (membership == membership')
              && checkResignation txInfoSignatories user delegation delegation'
    ResignMembership user ->
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkDatumModificationTx
          txInfoTxCerts
          datumIn
          \membership membership' delegation delegation' ->
            traceIfFalse "Delegation not conserved" (delegation == delegation')
              && checkResignation txInfoSignatories user membership membership'
    ResignCold ->
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkCertifyingTx
          membershipUsers
          (TxCertResignColdCommittee coldCred)
          txInfoTxCerts
          txInfoSignatories
          datumIn
    RotateCold ->
      checkContinuingTx inAddress inValue txInfoOutputs
        $ checkDatumModificationTx
          txInfoTxCerts
          datumIn
          \membership membership' delegation delegation' ->
            checkMultiSig membership txInfoSignatories
              && checkRotation txInfoSignatories membership membership'
              && checkRotation txInfoSignatories delegation delegation'
    BurnCold ->
      checkTerminalTx txInfoTxCerts txInfoSignatories datumIn
        && checkBurn coldNFT txInfoOutputs
    UpgradeCold destination ->
      checkTerminalTx txInfoTxCerts txInfoSignatories datumIn
        && checkUpgrade coldNFT destination txInfoOutputs

{-# INLINEABLE checkCertifyingTx #-}
checkCertifyingTx
  :: (ColdLockDatum -> [Identity])
  -> TxCert
  -> [TxCert]
  -> [PubKeyHash]
  -> ColdLockDatum
  -> ColdLockDatum
  -> Bool
checkCertifyingTx authGroup expectedCert certs signatories datumIn datumOut =
  traceIfFalse "Own datum not conserved" (datumIn == datumOut)
    && checkMultiSig (authGroup datumIn) signatories
    && traceIfFalse "Unexpected certificates" (certs == [expectedCert])

{-# INLINEABLE checkDatumModificationTx #-}
checkDatumModificationTx
  :: [TxCert]
  -> ColdLockDatum
  -> ([Identity] -> [Identity] -> [Identity] -> [Identity] -> Bool)
  -> ColdLockDatum
  -> Bool
checkDatumModificationTx
  certs
  (ColdLockDatum ca membership delegation)
  checkDiff
  (ColdLockDatum ca' membership' delegation') =
    traceIfFalse "CA not conserved" (ca == ca')
      && traceIfFalse "Tx publishes certificates" (null certs)
      && checkDiff membership membership' delegation delegation'

{-# INLINEABLE checkTerminalTx #-}
checkTerminalTx :: [TxCert] -> [PubKeyHash] -> ColdLockDatum -> Bool
checkTerminalTx certs signatories (ColdLockDatum _ membership _) =
  checkMultiSig membership signatories
    && traceIfFalse "Tx publishes certificates" (null certs)
