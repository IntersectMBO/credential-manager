{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module CredentialManager.Scripts.Common where

import CredentialManager.Api (Identity (..))
import PlutusLedgerApi.V3 (Address (..), PubKeyHash, TxOut (..))
import PlutusTx.Prelude

{-# INLINEABLE checkMultiSig #-}
checkMultiSig :: [Identity] -> [PubKeyHash] -> Bool
checkMultiSig [] _ = False
checkMultiSig list signatures = majority <= numberOfSignatures
  where
    allSignatures = nub $ pubKeyHash <$> list
    majority = (\x -> divide x 2 + modulo x 2) $ length allSignatures
    numberOfSignatures = length $ filter (`elem` signatures) allSignatures

{-# INLINEABLE ownOutputs #-}
ownOutputs :: Address -> [TxOut] -> [TxOut]
ownOutputs (Address ownCredential _) =
  filter \(TxOut (Address outCredential _) _ _ _) ->
    ownCredential == outCredential

{-# INLINEABLE checkTxOutPreservation #-}
checkTxOutPreservation :: TxOut -> [TxOut] -> Bool
checkTxOutPreservation (TxOut addrIn valueIn datumIn _) outputs =
  case ownOutputs addrIn outputs of
    [TxOut addrOut valueOut datumOut _] ->
      addrIn == addrOut && valueIn == valueOut && datumIn == datumOut
    _ -> False
