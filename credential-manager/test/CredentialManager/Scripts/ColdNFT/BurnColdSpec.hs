module CredentialManager.Scripts.ColdNFT.BurnColdSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFT
import CredentialManager.Scripts.ColdNFTSpec (nonMembershipSigners)
import CredentialManager.Scripts.HotNFTSpec (hasToken)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValue)
import PlutusLedgerApi.V3 (
  Address,
  ColdCommitteeCredential,
  Datum (..),
  OutputDatum (..),
  ScriptPurpose (..),
  ToData (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  Value,
 )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop
    "Invariant BC1: BurnCold fails if signed by minority of membership group"
    invariantBC1BurnColdMembershipMinority
  prop
    "Invariant BC2: BurnCold ignores duplicate signers in membership group"
    invariantBC2DuplicateMembership
  prop
    "Invariant BC3: BurnCold fails if membership list is empty"
    invariantBC3EmptyMembership
  prop
    "Invariant BC4: BurnCold fails if token not burned"
    invariantBC4NotBurned
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript coldNFT unlockColdCredential datum redeemer ctx === True

invariantBC1BurnColdMembershipMinority :: ValidArgs -> Property
invariantBC1BurnColdMembershipMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let allSigners = nub $ pubKeyHash <$> membershipUsers datum
    let minSigners = succ (length allSigners) `div` 2
    forAllShrink (chooseInt (0, pred minSigners)) shrink \signerCount ->
      forAll (nonMembershipSigners datum) \extraSigners -> do
        membershipSigners <- take signerCount <$> shuffle allSigners
        signers <- shuffle $ membershipSigners <> extraSigners
        let ctx' =
              ctx
                { scriptContextTxInfo =
                    (scriptContextTxInfo ctx)
                      { txInfoSignatories = signers
                      }
                }
        pure $
          counterexample ("Signers: " <> show signers) $
            coldNFTScript coldNFT unlockColdCredential datum redeemer ctx' === False

invariantBC2DuplicateMembership :: ValidArgs -> Property
invariantBC2DuplicateMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let membershipGroup = membershipUsers datum
    let maybeChangeCertificateHash user =
          oneof
            [ pure user
            , do x <- arbitrary; pure user{certificateHash = x}
            ]
    duplicate <- traverse maybeChangeCertificateHash =<< sublistOf membershipGroup
    membershipUsers' <- shuffle $ membershipGroup <> duplicate
    let datum' = datum{membershipUsers = membershipUsers'}
    pure $
      counterexample ("Datum: " <> show datum') $
        coldNFTScript coldNFT unlockColdCredential datum' redeemer ctx === True

invariantBC3EmptyMembership :: ValidArgs -> Property
invariantBC3EmptyMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let datum' = datum{membershipUsers = []}
    coldNFTScript coldNFT unlockColdCredential datum' redeemer ctx === False

invariantBC4NotBurned :: ValidArgs -> Property
invariantBC4NotBurned args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    baseValue <- arbitrary
    let value = baseValue <> assetClassValue coldNFT 1
    output <-
      TxOut
        <$> arbitrary
        <*> pure value
        <*> arbitrary
        <*> arbitrary
    outputs' <- shuffle $ output : txInfoOutputs (scriptContextTxInfo ctx)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs = outputs'
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT unlockColdCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f datum BurnCold
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= unlockScriptRef) . txInInfoOutRef)
      inputs <- shuffle $ input : additionalInputs
      outputs <- listOf $ arbitrary `suchThat` (not . hasToken coldNFT)
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = unlockExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      info <-
        TxInfo inputs
          <$> arbitrary
          <*> pure outputs
          <*> arbitrary
          <*> arbitrary
          <*> pure []
          <*> arbitrary
          <*> arbitrary
          <*> pure signers
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
      pure $ ScriptContext info $ Spending unlockScriptRef
    shrink' ScriptContext{..} =
      ScriptContext
        <$> shrinkInfo scriptContextTxInfo
        <*> pure scriptContextPurpose
    shrinkInfo TxInfo{..} =
      fold
        [ [TxInfo{txInfoInputs = x, ..} | x <- shrinkInputs txInfoInputs]
        , [TxInfo{txInfoReferenceInputs = x, ..} | x <- shrink txInfoReferenceInputs]
        , [TxInfo{txInfoOutputs = x, ..} | x <- shrink txInfoOutputs]
        , [TxInfo{txInfoFee = x, ..} | x <- shrink txInfoFee]
        , [TxInfo{txInfoMint = x, ..} | x <- shrink txInfoMint]
        , [TxInfo{txInfoWdrl = x, ..} | x <- shrink txInfoWdrl]
        , [TxInfo{txInfoValidRange = x, ..} | x <- shrink txInfoValidRange]
        , [TxInfo{txInfoRedeemers = x, ..} | x <- shrink txInfoRedeemers]
        , [TxInfo{txInfoData = x, ..} | x <- shrink txInfoData]
        , [TxInfo{txInfoId = x, ..} | x <- shrink txInfoId]
        , [TxInfo{txInfoVotes = x, ..} | x <- shrink txInfoVotes]
        , [TxInfo{txInfoTxCerts = x, ..} | x <- shrink txInfoTxCerts]
        , [ TxInfo{txInfoProposalProcedures = x, ..} | x <- shrink txInfoProposalProcedures
          ]
        , [ TxInfo{txInfoCurrentTreasuryAmount = x, ..}
          | x <- shrink txInfoCurrentTreasuryAmount
          ]
        , [TxInfo{txInfoTreasuryDonation = x, ..} | x <- shrink txInfoTreasuryDonation]
        ]
    shrinkInputs [] = []
    shrinkInputs (input' : ins)
      | input' == input = (input' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink input'
            , (input' :) <$> shrink ins
            , pure ins
            ]
    allSigners = nubBy (on (==) pubKeyHash) $ membershipUsers datum
    datum =
      ColdLockDatum
        { certificateAuthority = unlockCA
        , membershipUsers =
            unlockMembershipPre
              <> (unlockExtraMembership : unlockMembershipPost)
        , delegationUsers = unlockDelegation
        }
    input =
      TxInInfo unlockScriptRef $
        TxOut
          unlockScriptAddress
          unlockValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing

data ValidArgs = ValidArgs
  { coldNFT :: AssetClass
  , unlockScriptRef :: TxOutRef
  , unlockScriptAddress :: Address
  , unlockValue :: Value
  , unlockCA :: Identity
  , unlockMembershipPre :: [Identity]
  , unlockExtraMembership :: Identity
  , unlockMembershipPost :: [Identity]
  , unlockDelegation :: [Identity]
  , unlockColdCredential :: ColdCommitteeCredential
  , unlockExcessSignatureFraction :: Fraction
  }
  deriving (Show, Eq, Generic)

instance Arbitrary ValidArgs where
  arbitrary =
    ValidArgs
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink
