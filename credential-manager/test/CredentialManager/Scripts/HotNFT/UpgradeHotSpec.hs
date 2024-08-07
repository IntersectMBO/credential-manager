module CredentialManager.Scripts.HotNFT.UpgradeHotSpec where

import CredentialManager.Api
import CredentialManager.Gen (
  Fraction (..),
  genNonAdaAssetClass,
 )
import CredentialManager.Scripts.ColdNFTSpec (
  importanceSampleScriptValue,
  nonDelegationSigners,
 )
import CredentialManager.Scripts.HotNFT
import CredentialManager.Scripts.HotNFTSpec (hasToken)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  Value (..),
  assetClassValue,
  assetClassValueOf,
 )
import PlutusLedgerApi.V3 (
  Address (..),
  ColdCommitteeCredential,
  Credential (..),
  Datum (..),
  HotCommitteeCredential,
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
 )
import qualified PlutusTx.AssocMap as AMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop
    "Invariant UH1: UpgradeHot fails if signed by minority of delegation group"
    invariantUH1UpgradeHotDelegationMinority
  prop
    "Invariant UH2: UpgradeHot ignores duplicate signers in delegation group"
    invariantUH2DuplicateDelegation
  prop
    "Invariant UH3: UpgradeHot fails if delegation list is empty"
    invariantUH3EmptyDelegation
  prop
    "Invariant UH4: UpgradeHot fails if the cold script reference input is missing"
    invariantUH4ColdRefMissing
  prop
    "Invariant UH5: UpgradeHot fails if token burned"
    invariantUH5Burned
  prop
    "Invariant UH6: UpgradeHot fails if token sent to wrong address"
    invariantUH6WrongAddress
  prop
    "Invariant UH7: UpgradeHot fails if upgrades to the same script"
    invariantUH7SameScript
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \coldNFT hotNFT _ _ _ ctx ->
        hotNFTScript coldNFT hotNFT upgradeHotCredential ctx === True

invariantUH1UpgradeHotDelegationMinority :: ValidArgs -> Property
invariantUH1UpgradeHotDelegationMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum _ _ ctx -> do
    let allSigners = nub $ pubKeyHash <$> delegationUsers coldDatum
    let minSigners = succ (length allSigners) `div` 2
    forAllShrink (chooseInt (0, pred minSigners)) shrink \signerCount ->
      forAll (nonDelegationSigners coldDatum) \extraSigners -> do
        delegationSigners <- take signerCount <$> shuffle allSigners
        signers <- shuffle $ delegationSigners <> extraSigners
        let ctx' =
              ctx
                { scriptContextTxInfo =
                    (scriptContextTxInfo ctx)
                      { txInfoSignatories = signers
                      }
                }
        pure $
          counterexample ("Signers: " <> show signers) $
            hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === False

invariantUH2DuplicateDelegation :: ValidArgs -> Property
invariantUH2DuplicateDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum _ _ ctx -> do
    let delegationGroup = delegationUsers coldDatum
    let maybeChangeCertificateHash user =
          oneof
            [ pure user
            , do x <- arbitrary; pure user{certificateHash = x}
            ]
    duplicate <- traverse maybeChangeCertificateHash =<< sublistOf delegationGroup
    delegationUsers' <- shuffle $ delegationGroup <> duplicate
    let newDatum = coldDatum{delegationUsers = delegationUsers'}
    let modifyDatum (TxInInfo ref TxOut{..})
          | assetClassValueOf txOutValue coldNFT /= 0 =
              TxInInfo
                ref
                TxOut
                  { txOutDatum = OutputDatum $ Datum $ toBuiltinData newDatum
                  , ..
                  }
          | otherwise = TxInInfo ref TxOut{..}
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoReferenceInputs =
                      map modifyDatum $
                        txInfoReferenceInputs $
                          scriptContextTxInfo ctx
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === True

invariantUH3EmptyDelegation :: ValidArgs -> Property
invariantUH3EmptyDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum _ _ ctx -> do
    let newDatum = coldDatum{delegationUsers = []}
    let modifyDatum (TxInInfo ref TxOut{..})
          | assetClassValueOf txOutValue coldNFT /= 0 =
              TxInInfo
                ref
                TxOut
                  { txOutDatum = OutputDatum $ Datum $ toBuiltinData newDatum
                  , ..
                  }
          | otherwise = TxInInfo ref TxOut{..}
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoReferenceInputs =
                      map modifyDatum $
                        txInfoReferenceInputs $
                          scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === False

invariantUH4ColdRefMissing :: ValidArgs -> Property
invariantUH4ColdRefMissing args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ _ _ ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoReferenceInputs =
                      filter (not . hasToken coldNFT . txInInfoResolved) $
                        txInfoReferenceInputs $
                          scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === False

invariantUH5Burned :: ValidArgs -> Property
invariantUH5Burned args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ _ _ ctx -> do
    let outputs' = filter (not . hasToken hotNFT) $ txInfoOutputs $ scriptContextTxInfo ctx
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs = outputs'
                  }
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === False

invariantUH6WrongAddress :: ValidArgs -> Property
invariantUH6WrongAddress args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ _ _ ctx -> do
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
        hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === False

invariantUH7SameScript :: ValidArgs -> Property
invariantUH7SameScript args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ _ _ ctx -> do
    let destinationScriptCredential = case addressCredential upgradeScriptAddress of
          ScriptCredential source -> source
          _ -> error "ValidArgs should have a script address as a source"
    destinationStakingCredential <- arbitrary
    let destination =
          Address
            (ScriptCredential destinationScriptCredential)
            destinationStakingCredential
        tweakAddress TxOut{..}
          | addressCredential txOutAddress == ScriptCredential upgradeDestination = do
              pure TxOut{txOutAddress = destination, ..}
          | otherwise = pure TxOut{..}
    outputs' <- traverse tweakAddress $ txInfoOutputs $ scriptContextTxInfo ctx
    let redeemer = UpgradeHot destinationScriptCredential
        ctx' =
          ctx
            { scriptContextRedeemer = Redeemer $ toBuiltinData redeemer
            , scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs = outputs'
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT upgradeHotCredential ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> ( AssetClass
       -> AssetClass
       -> ColdLockDatum
       -> HotLockDatum
       -> HotLockRedeemer
       -> ScriptContext
       -> prop
     )
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $
    f upgradeColdNFT upgradeHotNFT inColdDatum datum redeemer
  where
    redeemer = UpgradeHot upgradeDestination
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= upgradeScriptRef) . txInInfoOutRef)
      inputs <- shuffle $ input : additionalInputs
      destination <- Address (ScriptCredential upgradeDestination) <$> arbitrary
      baseValue <- arbitrary `suchThat` \v -> assetClassValueOf v upgradeHotNFT == 0
      let outputValue = baseValue <> assetClassValue upgradeHotNFT 1
      output <- TxOut destination outputValue <$> arbitrary <*> arbitrary
      extraOutputs <-
        listOf $
          arbitrary `suchThat` \out@TxOut{..} ->
            not (hasToken upgradeHotNFT out)
              && (addressCredential txOutAddress /= ScriptCredential upgradeDestination)
      additionalRefInputs <-
        listOf $ arbitrary `suchThat` (not . hasToken upgradeColdNFT . txInInfoResolved)
      outputs <- shuffle $ output : extraOutputs
      refInputs <- shuffle $ refInput : additionalRefInputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = upgradeExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      info <-
        TxInfo inputs refInputs outputs
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> pure signers
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> pure AMap.empty
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
    allSigners = nubBy (on (==) pubKeyHash) $ delegationUsers inColdDatum
    inColdDatum =
      ColdLockDatum
        { certificateAuthority = upgradeCA
        , membershipUsers = upgradeMembership
        , delegationUsers =
            upgradeDelegationPre
              <> (upgradeExtraDelegation : upgradeDelegationPost)
        }
    datum =
      HotLockDatum
        { votingUsers = upgradeVoting
        }
    input =
      TxInInfo upgradeScriptRef $
        TxOut
          upgradeScriptAddress
          upgradeValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing
    AssetClass (coldPolicy, coldName) = upgradeColdNFT
    refInput =
      TxInInfo upgradeColdScriptRef $
        TxOut
          upgradeColdScriptAddress
          ( Value $
              AMap.insert coldPolicy (AMap.singleton coldName 1) $
                getValue upgradeColdValue
          )
          (OutputDatum $ Datum $ toBuiltinData inColdDatum)
          Nothing

data ValidArgs = ValidArgs
  { upgradeHotNFT :: AssetClass
  , upgradeColdNFT :: AssetClass
  , upgradeHotCredential :: HotCommitteeCredential
  , upgradeDestination :: ScriptHash
  , upgradeColdScriptRef :: TxOutRef
  , upgradeColdScriptAddress :: Address
  , upgradeScriptRef :: TxOutRef
  , upgradeScriptAddress :: Address
  , upgradeValue :: Value
  , upgradeColdValue :: Value
  , upgradeCA :: Identity
  , upgradeDelegationPre :: [Identity]
  , upgradeExtraDelegation :: Identity
  , upgradeDelegationPost :: [Identity]
  , upgradeMembership :: [Identity]
  , upgradeVoting :: [Identity]
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
    hotNFT <- genNonAdaAssetClass `suchThat` (/= coldNFT)
    upgradeValue <- importanceSampleScriptValue True hotNFT
    ValidArgs hotNFT coldNFT
      <$> arbitrary
      <*> pure destination
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure scriptAddress
      <*> pure upgradeValue
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
