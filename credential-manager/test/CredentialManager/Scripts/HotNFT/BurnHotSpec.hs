module CredentialManager.Scripts.HotNFT.BurnHotSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFTSpec (nonDelegationSigners)
import CredentialManager.Scripts.HotNFT
import CredentialManager.Scripts.HotNFTSpec (hasToken)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClassValue,
  assetClassValueOf,
 )
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  HotCommitteeCredential,
  OutputDatum (..),
  ScriptPurpose (..),
  ToData (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  Value (..),
 )
import qualified PlutusTx.AssocMap as AMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop
    "Invariant BH1: BurnHot fails if signed by minority of delegation group"
    invariantBH1BurnHotDelegationMinority
  prop
    "Invariant BH2: BurnHot ignores duplicate signers in delegation group"
    invariantBH2DuplicateDelegation
  prop
    "Invariant BH3: BurnHot fails if delegation list is empty"
    invariantBH3EmptyDelegation
  prop
    "Invariant BH4: BurnHot fails if the cold script reference input is missing"
    invariantBH4ColdRefMissing
  prop
    "Invariant BH5: BurnHot fails if token is not burned"
    invariantBH5NotBurned
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx ->
        hotNFTScript coldNFT hotNFT unlockHotCredential datum redeemer ctx === True

invariantBH1BurnHotDelegationMinority :: ValidArgs -> Property
invariantBH1BurnHotDelegationMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum datum redeemer ctx -> do
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
            hotNFTScript coldNFT hotNFT unlockHotCredential datum redeemer ctx' === False

invariantBH2DuplicateDelegation :: ValidArgs -> Property
invariantBH2DuplicateDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum datum redeemer ctx -> do
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
        hotNFTScript coldNFT hotNFT unlockHotCredential datum redeemer ctx' === True

invariantBH3EmptyDelegation :: ValidArgs -> Property
invariantBH3EmptyDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum datum redeemer ctx -> do
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
      hotNFTScript coldNFT hotNFT unlockHotCredential datum redeemer ctx' === False

invariantBH4ColdRefMissing :: ValidArgs -> Property
invariantBH4ColdRefMissing args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
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
      hotNFTScript coldNFT hotNFT unlockHotCredential datum redeemer ctx' === False

invariantBH5NotBurned :: ValidArgs -> Property
invariantBH5NotBurned args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
    baseValue <- arbitrary
    let value = baseValue <> assetClassValue hotNFT 1
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
        hotNFTScript coldNFT hotNFT unlockHotCredential datum redeemer ctx' === False

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
    f unlockColdNFT unlockHotNFT inColdDatum datum BurnHot
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= unlockScriptRef) . txInInfoOutRef)
      additionalRefInputs <-
        listOf $ arbitrary `suchThat` (not . hasToken unlockColdNFT . txInInfoResolved)
      outputs <- listOf $ arbitrary `suchThat` (not . hasToken unlockHotNFT)
      inputs <- shuffle $ input : additionalInputs
      refInputs <- shuffle $ refInput : additionalRefInputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = unlockExcessSignatureFraction
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
      pure $ ScriptContext info $ Spending unlockScriptRef
    shrink' ScriptContext{..} =
      ScriptContext
        <$> shrinkInfo scriptContextTxInfo
        <*> pure scriptContextPurpose
    shrinkInfo TxInfo{..} =
      fold
        [ [TxInfo{txInfoInputs = x, ..} | x <- shrinkInputs txInfoInputs]
        , [ TxInfo{txInfoReferenceInputs = x, ..}
          | x <- shrinkRefInputs txInfoReferenceInputs
          ]
        , [TxInfo{txInfoOutputs = x, ..} | x <- shrink txInfoOutputs]
        , [TxInfo{txInfoFee = x, ..} | x <- shrink txInfoFee]
        , [TxInfo{txInfoMint = x, ..} | x <- shrink txInfoMint]
        , [TxInfo{txInfoWdrl = x, ..} | x <- shrink txInfoWdrl]
        , [TxInfo{txInfoValidRange = x, ..} | x <- shrink txInfoValidRange]
        , [TxInfo{txInfoRedeemers = x, ..} | x <- shrink txInfoRedeemers]
        , [TxInfo{txInfoData = x, ..} | x <- shrink txInfoData]
        , [TxInfo{txInfoId = x, ..} | x <- shrink txInfoId]
        , [TxInfo{txInfoVotes = x, ..} | x <- shrink txInfoVotes]
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
    shrinkRefInputs [] = []
    shrinkRefInputs (input' : ins)
      | input' == refInput = (input' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink input'
            , (input' :) <$> shrink ins
            , pure ins
            ]
    allSigners = nubBy (on (==) pubKeyHash) $ delegationUsers inColdDatum
    inColdDatum =
      ColdLockDatum
        { certificateAuthority = unlockCA
        , membershipUsers = unlockMembership
        , delegationUsers =
            unlockDelegationPre
              <> (unlockExtraDelegation : unlockDelegationPost)
        }
    datum = HotLockDatum unlockVoting
    input =
      TxInInfo unlockScriptRef $
        TxOut
          unlockScriptAddress
          unlockValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing
    AssetClass (coldPolicy, coldName) = unlockColdNFT
    refInput =
      TxInInfo unlockColdScriptRef $
        TxOut
          unlockColdScriptAddress
          ( Value $
              AMap.insert coldPolicy (AMap.singleton coldName 1) $
                getValue unlockColdValue
          )
          (OutputDatum $ Datum $ toBuiltinData inColdDatum)
          Nothing

data ValidArgs = ValidArgs
  { unlockScriptRef :: TxOutRef
  , unlockScriptAddress :: Address
  , unlockColdScriptRef :: TxOutRef
  , unlockColdScriptAddress :: Address
  , unlockValue :: Value
  , unlockColdValue :: Value
  , unlockCA :: Identity
  , unlockMembership :: [Identity]
  , unlockDelegationPre :: [Identity]
  , unlockExtraDelegation :: Identity
  , unlockDelegationPost :: [Identity]
  , unlockVoting :: [Identity]
  , unlockColdNFT :: AssetClass
  , unlockHotNFT :: AssetClass
  , unlockHotCredential :: HotCommitteeCredential
  , unlockExcessSignatureFraction :: Fraction
  }
  deriving (Show, Eq, Generic)

instance Arbitrary ValidArgs where
  arbitrary = do
    coldNFT <- arbitrary
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
      <*> arbitrary
      <*> pure coldNFT
      <*> arbitrary `suchThat` (/= coldNFT)
      <*> arbitrary
      <*> arbitrary
  shrink = filter (not . invalid) . genericShrink
    where
      invalid ValidArgs{..} = unlockColdNFT == unlockHotNFT
