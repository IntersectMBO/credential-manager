module CredentialManager.Scripts.ColdNFTSpec where

import CredentialManager.Api
import CredentialManager.Gen ()
import CredentialManager.Scripts.ColdNFT
import Data.List (nub)
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential,
  PubKeyHash,
 )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "Invariant 1: Fails if not spending" invariant1BadPurpose

invariant1BadPurpose
  :: ColdCommitteeCredential
  -> ColdLockDatum
  -> ColdLockRedeemer
  -> ScriptContext
  -> Property
invariant1BadPurpose coldCredential datum redeemer ctx = case scriptContextPurpose ctx of
  Spending{} -> discard
  _ -> coldNFTScript coldCredential datum redeemer ctx === False

nonDelegationSigners :: ColdLockDatum -> Gen [PubKeyHash]
nonDelegationSigners ColdLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash delegationUsers))

nonMembershipSigners :: ColdLockDatum -> Gen [PubKeyHash]
nonMembershipSigners ColdLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash membershipUsers))
