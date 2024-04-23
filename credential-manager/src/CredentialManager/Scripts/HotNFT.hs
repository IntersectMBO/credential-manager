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
  Identity (..),
 )
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (..),
  FromData (..),
  GovernanceActionId,
  Map,
  OutputDatum (..),
  PubKeyHash,
  ToData (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  Value (..),
  Voter (..),
  unsafeFromBuiltinData,
 )
import qualified PlutusLedgerApi.V3 as PV3
import PlutusLedgerApi.V3.Contexts (HotCommitteeCredential)
import PlutusTx.AssocMap (member)
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

-- | Find the reference input that contains a certain currency symbol.
{-# INLINEABLE findTxInByCurrencySymbolInRefUTxO #-}
findTxInByCurrencySymbolInRefUTxO :: CurrencySymbol -> TxInfo -> Maybe TxInInfo
findTxInByCurrencySymbolInRefUTxO symbol txInfo =
  find
    (\txIn -> symbol `member` (getValue . txOutValue . txInInfoResolved) txIn)
    (txInfoReferenceInputs txInfo)

-- | This script just checks that the hard-coded currency symbol of the NFT is
-- in any spending input of the transaction.
{-# INLINEABLE hotNFTScript #-}
hotNFTScript
  :: CurrencySymbol
  -> CurrencySymbol
  -> HotCommitteeCredential
  -> HotLockDatum
  -> HotLockRedeemer
  -> ScriptContext
  -> Bool
hotNFTScript coldPolicyId hotPolicyId hotCred (HotLockDatum votingUsers) red ctx =
  case scriptContextPurpose ctx of
    Spending txOurRef -> case findTxInByTxOutRef txOurRef txInfo of
      Nothing -> False
      Just (TxInInfo _ ownInput) -> case red of
        Vote govAction vote ->
          checkTxOutPreservation
            && checkMultiSig votingUsers
            && checkVote
          where
            checkTxOutPreservation =
              ownInput `elem` txInfoOutputs txInfo
            checkVote =
              txInfoVotes txInfo
                == Map.singleton
                  (CommitteeVoter hotCred)
                  (Map.singleton govAction vote)
        ResignVoting user ->
          isVotingUser
            && signedByResignee
            && resigneeRemoved
            && checkNoVotes
          where
            isVotingUser = user `elem` votingUsers
            signedByResignee = txSignedBy txInfo $ pubKeyHash user
            newDatum = HotLockDatum (filter (/= user) votingUsers)
            resigneeRemoved =
              let newTxOutput =
                    ownInput
                      { txOutDatum =
                          OutputDatum $ Datum $ toBuiltinData newDatum
                      }
               in newTxOutput `elem` txInfoOutputs txInfo
            checkNoVotes = Map.null $ txInfoVotes txInfo
        RotateHot ->
          checkOutput
            && signedByDelegators
            && checkNoVotes
          where
            toSelf (TxOut address _ _ _) = txOutAddress ownInput == address
            ownOutputs = filter toSelf $ txInfoOutputs txInfo
            checkOutput = case ownOutputs of
              [TxOut _ value' (OutputDatum (Datum datum')) Nothing] ->
                let HotLockDatum voting' =
                      unsafeFromBuiltinData datum'
                 in not (null voting') && value' == txOutValue ownInput
              _ -> False
            checkNoVotes = Map.null $ txInfoVotes txInfo
            signedByDelegators =
              case findTxInByCurrencySymbolInRefUTxO coldPolicyId txInfo of
                Nothing -> False
                Just (TxInInfo _ refInput) -> case txOutDatum refInput of
                  OutputDatum datum -> case fromBuiltinData $ getDatum datum of
                    Just ColdLockDatum{..} -> checkMultiSig delegationUsers
                    _ -> False
                  _ -> False
        BurnHot -> signedByDelegators && checkBurn
          where
            signedByDelegators =
              case findTxInByCurrencySymbolInRefUTxO coldPolicyId txInfo of
                Nothing -> False
                Just (TxInInfo _ refInput) -> case txOutDatum refInput of
                  OutputDatum datum -> case fromBuiltinData $ getDatum datum of
                    Just ColdLockDatum{..} -> checkMultiSig delegationUsers
                    _ -> False
                  _ -> False
            checkBurn = not $ any outputContainsNFT $ txInfoOutputs txInfo
            outputContainsNFT txOut =
              hotPolicyId `member` getValue (txOutValue txOut)
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
