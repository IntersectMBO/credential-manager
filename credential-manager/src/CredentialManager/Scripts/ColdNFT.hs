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
  Identity (..),
 )
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  PubKeyHash,
  ToData (..),
  TxCert (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  unsafeFromBuiltinData,
 )
import PlutusLedgerApi.V3.Contexts (ColdCommitteeCredential)
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
  deriving stock (Haskell.Eq, Haskell.Show)

-- | A version of PlutusLedgerApi.V3.ScriptPurpose that only decodes what the
-- cold NFT script needs.
data ScriptPurpose
  = Minting BuiltinData
  | Spending TxOutRef
  | Rewarding BuiltinData
  | Certifying BuiltinData BuiltinData
  | Voting BuiltinData
  | Proposing BuiltinData BuiltinData
  deriving stock (Haskell.Eq, Haskell.Show)

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
  deriving stock (Haskell.Show, Haskell.Eq)

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

-- | Check if a transaction was signed by the given public key.
{-# INLINEABLE txSignedBy #-}
txSignedBy :: TxInfo -> PubKeyHash -> Bool
txSignedBy TxInfo{txInfoSignatories} k = k `elem` txInfoSignatories

-- | This script just checks that the hard-coded currency symbol of the NFT is
-- in any spending input of the transaction.
{-# INLINEABLE coldNFTScript #-}
coldNFTScript
  :: ColdCommitteeCredential
  -> ColdLockDatum
  -> ColdLockRedeemer
  -> ScriptContext
  -> Bool
coldNFTScript coldCred (ColdLockDatum ca membershipUsers delegationUsers) red ctx =
  case scriptContextPurpose ctx of
    Spending txOurRef -> case txInInfoResolved <$> findTxInByTxOutRef txOurRef txInfo of
      Nothing -> False
      Just ownInput -> case red of
        AuthorizeHot hotCred ->
          checkTxOutPreservation
            && checkMultiSig delegationUsers
            && checkAuthHotCert
          where
            checkTxOutPreservation =
              ownInput `elem` txInfoOutputs txInfo
            checkAuthHotCert =
              txInfoTxCerts txInfo == [TxCertAuthHotCommittee coldCred hotCred]
        ResignDelegation user ->
          isDelegationUser
            && signedByResignee
            && resigneeRemoved
            && checkNoCerts
          where
            isDelegationUser = user `elem` delegationUsers
            signedByResignee = txSignedBy txInfo $ pubKeyHash user
            newDatum =
              ColdLockDatum
                ca
                membershipUsers
                (filter (/= user) delegationUsers)
            resigneeRemoved =
              let newTxOutput =
                    ownInput
                      { txOutDatum =
                          OutputDatum $ Datum $ toBuiltinData newDatum
                      }
               in newTxOutput `elem` txInfoOutputs txInfo
            checkNoCerts = null $ txInfoTxCerts txInfo
        ResignCold ->
          checkTxOutPreservation
            && checkMultiSig membershipUsers
            && checkResignationCert
          where
            checkTxOutPreservation =
              ownInput `elem` txInfoOutputs txInfo
            checkResignationCert =
              txInfoTxCerts txInfo == [TxCertResignColdCommittee coldCred]
        RotateCold ->
          checkOutput
            && checkMultiSig membershipUsers
            && checkNoCerts
          where
            toSelf (TxOut address _ _ _) = txOutAddress ownInput == address
            ownOutputs = filter toSelf $ txInfoOutputs txInfo
            checkOutput = case ownOutputs of
              [TxOut _ value' (OutputDatum (Datum datum')) Nothing] ->
                let ColdLockDatum ca' membership' delegation' =
                      unsafeFromBuiltinData datum'
                 in (ca' == ca)
                      && not (null membership')
                      && not (null delegation')
                      && value'
                      == txOutValue ownInput
              _ -> False
            checkNoCerts = null $ txInfoTxCerts txInfo
        Unlock -> checkMultiSig membershipUsers
    _ -> False
  where
    txInfo = scriptContextTxInfo ctx
    checkMultiSig list =
      majority <= numberOfSignatures && numberOfSignatures > 0
      where
        majority = (\x -> divide x 2 + modulo x 2) $ length list
        numberOfSignatures =
          length $ filter (txSignedBy txInfo . pubKeyHash) list

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
