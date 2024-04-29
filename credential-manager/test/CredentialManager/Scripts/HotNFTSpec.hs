module CredentialManager.Scripts.HotNFTSpec where

import CredentialManager.Api
import CredentialManager.Gen ()
import CredentialManager.Scripts.HotNFT
import Data.List (nub)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  HotCommitteeCredential,
  PubKeyHash,
 )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "Invariant 1: Fails if not spending" invariant1BadPurpose

invariant1BadPurpose
  :: CurrencySymbol
  -> HotCommitteeCredential
  -> HotLockDatum
  -> HotLockRedeemer
  -> ScriptContext
  -> Property
invariant1BadPurpose coldPolicy hotCred datum redeemer ctx = case scriptContextPurpose ctx of
  Spending{} -> discard
  _ -> hotNFTScript coldPolicy hotCred datum redeemer ctx === False

nonVotingSigners :: HotLockDatum -> Gen [PubKeyHash]
nonVotingSigners HotLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash votingUsers))
