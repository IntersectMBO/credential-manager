module CredentialManager.Scripts.ColdNFT.UpgradeColdSpec where

import CredentialManager.Api
import CredentialManager.Gen (
  Fraction (..),
  genNonAdaAssetClass,
 )
import CredentialManager.Scripts.ColdNFT
import CredentialManager.Scripts.ColdNFT.RotateColdSpec (updateDatum)
import CredentialManager.Scripts.ColdNFTSpec (
  importanceSampleScriptValue,
  nonMembershipSigners,
 )
import CredentialManager.Scripts.HotNFTSpec (hasToken)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValue, assetClassValueOf)
import PlutusLedgerApi.V3 (
  Address (..),
  ColdCommitteeCredential,
  Credential (..),
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  ToData (..),
  TxInInfo (..),
  TxInfo (..),
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
    "Invariant UC1: UpgradeCold fails if signed by minority of membership group"
    invariantUC1UpgradeColdMembershipMinority
  prop
    "Invariant UC2: UpgradeCold ignores duplicate signers in membership group"
    invariantUC2DuplicateMembership
  prop
    "Invariant UC3: UpgradeCold fails if membership list is empty"
    invariantUC3EmptyMembership
  prop
    "Invariant UC4: UpgradeCold fails if token burned"
    invariantUC4Burned
  prop
    "Invariant UC5: UpgradeCold fails if token sent to wrong address"
    invariantUC5WrongAddress
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \_ _ ctx ->
        coldNFTScript coldNFT upgradeColdCredential ctx === True

invariantUC1UpgradeColdMembershipMinority :: ValidArgs -> Property
invariantUC1UpgradeColdMembershipMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \datum _ ctx -> do
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
            coldNFTScript coldNFT upgradeColdCredential ctx' === False

invariantUC2DuplicateMembership :: ValidArgs -> Property
invariantUC2DuplicateMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum _ ctx -> do
    let membershipGroup = membershipUsers datum
    let maybeChangeCertificateHash user =
          oneof
            [ pure user
            , do x <- arbitrary; pure user{certificateHash = x}
            ]
    duplicate <- traverse maybeChangeCertificateHash =<< sublistOf membershipGroup
    membershipUsers' <- shuffle $ membershipGroup <> duplicate
    let datum' = datum{membershipUsers = membershipUsers'}
    let ctx' = updateDatum datum' ctx
    pure $
      counterexample ("Datum: " <> show datum') $
        coldNFTScript coldNFT upgradeColdCredential ctx' === True

invariantUC3EmptyMembership :: ValidArgs -> Property
invariantUC3EmptyMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum _ ctx -> do
    let datum' = datum{membershipUsers = []}
    let ctx' = updateDatum datum' ctx
    coldNFTScript coldNFT upgradeColdCredential ctx' === False

invariantUC4Burned :: ValidArgs -> Property
invariantUC4Burned args@ValidArgs{..} =
  forAllValidScriptContexts args \_ _ ctx -> do
    let outputs' = filter (not . hasToken coldNFT) $ txInfoOutputs $ scriptContextTxInfo ctx
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs = outputs'
                  }
            }
    counterexample ("Context: " <> show ctx') $
      coldNFTScript coldNFT upgradeColdCredential ctx' === False

invariantUC5WrongAddress :: ValidArgs -> Property
invariantUC5WrongAddress args@ValidArgs{..} =
  forAllValidScriptContexts args \_ _ ctx -> do
    let tweakAddress TxOut{..}
          | addressCredential txOutAddress == ScriptCredential upgradeDestination = do
              address <-
                arbitrary
                  `suchThat` ((/= ScriptCredential upgradeDestination) . addressCredential)
              pure TxOut{txOutAddress = address, ..}
          | otherwise = pure TxOut{..}
    outputs' <- traverse tweakAddress $ txInfoOutputs $ scriptContextTxInfo ctx
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs = outputs'
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT upgradeColdCredential ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f datum redeemer
  where
    redeemer = UpgradeCold upgradeDestination
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= upgradeScriptRef) . txInInfoOutRef)
      inputs <- shuffle $ input : additionalInputs
      destination <- Address (ScriptCredential upgradeDestination) <$> arbitrary
      baseValue <- arbitrary `suchThat` \v -> assetClassValueOf v coldNFT == 0
      let outputValue = baseValue <> assetClassValue coldNFT 1
      output <- TxOut destination outputValue <$> arbitrary <*> arbitrary
      extraOutputs <-
        listOf $
          arbitrary `suchThat` \out@TxOut{..} ->
            not (hasToken coldNFT out)
              && (addressCredential txOutAddress /= ScriptCredential upgradeDestination)
      outputs <- shuffle $ output : extraOutputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = upgradeExcessSignatureFraction
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
      let redeemer' = Redeemer $ toBuiltinData redeemer
      pure $
        ScriptContext info redeemer' $
          SpendingScript upgradeScriptRef $
            Just $
              Datum $
                toBuiltinData datum
    shrink' ScriptContext{..} =
      ScriptContext
        <$> shrinkInfo scriptContextTxInfo
        <*> pure scriptContextRedeemer
        <*> pure scriptContextScriptInfo
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
        { certificateAuthority = upgradeCA
        , membershipUsers =
            upgradeMembershipPre
              <> (upgradeExtraMembership : upgradeMembershipPost)
        , delegationUsers = upgradeDelegation
        }
    input =
      TxInInfo upgradeScriptRef $
        TxOut
          upgradeScriptAddress
          upgradeValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing

data ValidArgs = ValidArgs
  { coldNFT :: AssetClass
  , upgradeDestination :: ScriptHash
  , upgradeScriptRef :: TxOutRef
  , upgradeScriptAddress :: Address
  , upgradeValue :: Value
  , upgradeCA :: Identity
  , upgradeMembershipPre :: [Identity]
  , upgradeExtraMembership :: Identity
  , upgradeMembershipPost :: [Identity]
  , upgradeDelegation :: [Identity]
  , upgradeColdCredential :: ColdCommitteeCredential
  , upgradeExcessSignatureFraction :: Fraction
  }
  deriving (Show, Eq, Generic)

instance Arbitrary ValidArgs where
  arbitrary = do
    destination <- arbitrary
    scriptAddress <-
      arbitrary `suchThat` \addr -> case addressCredential addr of
        (ScriptCredential source) -> source /= destination
        _ -> False
    coldNFT <- genNonAdaAssetClass
    updradeValue <- importanceSampleScriptValue True coldNFT
    ValidArgs coldNFT destination
      <$> arbitrary
      <*> pure scriptAddress
      <*> pure updradeValue
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink
