module CredentialManager.Scripts.HotNFT.BurnHotSpec where

import CredentialManager.Api
import CredentialManager.Gen (
  Fraction (..),
  genNonAdaAssetClass,
  genTxInfoMintForNFTBurn,
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
  assetClassValue,
  assetClassValueOf,
 )
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  HotCommitteeCredential,
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptInfo (..),
  ToData (..),
  TxInInfo (..),
  TxInfo (..),
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
      forAllValidScriptContexts args \coldNFT hotNFT _ ctx ->
        hotNFTScript coldNFT hotNFT burnHotCredential ctx === True

invariantBH1BurnHotDelegationMinority :: ValidArgs -> Property
invariantBH1BurnHotDelegationMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum ctx -> do
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
            hotNFTScript coldNFT hotNFT burnHotCredential ctx' === False

invariantBH2DuplicateDelegation :: ValidArgs -> Property
invariantBH2DuplicateDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum ctx -> do
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
        hotNFTScript coldNFT hotNFT burnHotCredential ctx' === True

invariantBH3EmptyDelegation :: ValidArgs -> Property
invariantBH3EmptyDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT coldDatum ctx -> do
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
      hotNFTScript coldNFT hotNFT burnHotCredential ctx' === False

invariantBH4ColdRefMissing :: ValidArgs -> Property
invariantBH4ColdRefMissing args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ ctx -> do
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
      hotNFTScript coldNFT hotNFT burnHotCredential ctx' === False

invariantBH5NotBurned :: ValidArgs -> Property
invariantBH5NotBurned args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ ctx -> do
    baseValue <- arbitrary
    (nftOutValue, mintValue) <-
      elements
        [ (assetClassValue coldNFT 1, mempty) -- outputted not burned
        , (assetClassValue coldNFT 1, assetClassValue coldNFT (-1)) -- outputted and burned
        , (mempty, assetClassValue coldNFT 1) -- not outputted and minted
        , (mempty, mempty) -- not outputted and not burned/minted
        ]
    let value = baseValue <> nftOutValue
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
                  , txInfoMint = mintValue
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT burnHotCredential ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (AssetClass -> AssetClass -> ColdLockDatum -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $
    f burnColdNFT burnHotNFT inColdDatum
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= burnScriptRef) . txInInfoOutRef)
      additionalRefInputs <-
        listOf $ arbitrary `suchThat` (not . hasToken burnColdNFT . txInInfoResolved)
      outputs <- listOf $ arbitrary `suchThat` (not . hasToken burnHotNFT)
      inputs <- shuffle $ input : additionalInputs
      refInputs <- shuffle $ refInput : additionalRefInputs
      mint <- genTxInfoMintForNFTBurn burnHotNFT
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = burnExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      info <-
        TxInfo inputs refInputs outputs
          <$> arbitrary
          <*> pure mint
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
      let redeemer' = Redeemer $ toBuiltinData BurnHot
      pure $
        ScriptContext info redeemer' $
          SpendingScript burnScriptRef $
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
        { certificateAuthority = burnCA
        , membershipUsers = burnMembership
        , delegationUsers =
            burnDelegationPre
              <> (burnExtraDelegation : burnDelegationPost)
        }
    datum = HotLockDatum burnVoting
    input =
      TxInInfo burnScriptRef $
        TxOut
          burnScriptAddress
          burnValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing
    AssetClass (coldPolicy, coldName) = burnColdNFT
    refInput =
      TxInInfo burnColdScriptRef $
        TxOut
          burnColdScriptAddress
          ( Value $
              AMap.insert coldPolicy (AMap.singleton coldName 1) $
                getValue burnColdValue
          )
          (OutputDatum $ Datum $ toBuiltinData inColdDatum)
          Nothing

data ValidArgs = ValidArgs
  { burnScriptRef :: TxOutRef
  , burnScriptAddress :: Address
  , burnColdScriptRef :: TxOutRef
  , burnColdScriptAddress :: Address
  , burnValue :: Value
  , burnColdValue :: Value
  , burnCA :: Identity
  , burnMembership :: [Identity]
  , burnDelegationPre :: [Identity]
  , burnExtraDelegation :: Identity
  , burnDelegationPost :: [Identity]
  , burnVoting :: [Identity]
  , burnColdNFT :: AssetClass
  , burnHotNFT :: AssetClass
  , burnHotCredential :: HotCommitteeCredential
  , burnExcessSignatureFraction :: Fraction
  }
  deriving (Show, Eq, Generic)

instance Arbitrary ValidArgs where
  arbitrary = do
    coldNFT <- arbitrary
    hotNFT <- genNonAdaAssetClass `suchThat` (/= coldNFT)
    burnValue <- importanceSampleScriptValue True hotNFT
    ValidArgs
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure burnValue
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure coldNFT
      <*> pure hotNFT
      <*> arbitrary
      <*> arbitrary
  shrink = filter (not . invalid) . genericShrink
    where
      invalid ValidArgs{..} = burnColdNFT == burnHotNFT
