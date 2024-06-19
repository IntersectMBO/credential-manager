module CredentialManager.Scripts.HotNFT.RotateHotSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFTSpec (nonDelegationSigners)
import CredentialManager.Scripts.HotNFT
import CredentialManager.Scripts.HotNFTSpec (hasToken)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValueOf)
import PlutusLedgerApi.V3 (
  Address (..),
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
    "Invariant RTH1: RotateHot fails if signed by minority of delegation group"
    invariantRTH1RotateHotDelegationMinority
  prop
    "Invariant RTH2: RotateHot ignores duplicate signers in delegation group"
    invariantRTH2DuplicateDelegation
  prop
    "Invariant RTH3: RotateHot fails if delegation list is empty"
    invariantRTH3EmptyDelegation
  prop
    "Invariant RTH4: RotateHot fails if the cold script reference input is missing"
    invariantRTH4ColdRefMissing
  prop
    "Invariant RTH5: RotateHot fails if any votes are present"
    invariantRTH5ExtraneousVotes
  prop
    "Invariant RTH6: RotateHot fails without output to self"
    invariantRTH6NoSelfOutput
  prop
    "Invariant RTH7: RotateHot fails with multiple outputs to self"
    invariantRTH7MultipleSelfOutputs
  prop
    "Invariant RTH8: RotateHot fails if value not preserved"
    invariantRTH8ValueNotPreserved
  prop
    "Invariant RTH9: RotateHot fails if voting empty in output"
    invariantRTH9EmptyVotingOutput
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx ->
        hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx === True

invariantRTH1RotateHotDelegationMinority :: ValidArgs -> Property
invariantRTH1RotateHotDelegationMinority args@ValidArgs{..} =
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
            hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH2DuplicateDelegation :: ValidArgs -> Property
invariantRTH2DuplicateDelegation args@ValidArgs{..} =
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
        hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === True

invariantRTH3EmptyDelegation :: ValidArgs -> Property
invariantRTH3EmptyDelegation args@ValidArgs{..} =
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
      hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH4ColdRefMissing :: ValidArgs -> Property
invariantRTH4ColdRefMissing args@ValidArgs{..} =
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
      hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH5ExtraneousVotes :: ValidArgs -> Property
invariantRTH5ExtraneousVotes args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
    votes <-
      arbitrary `suchThat` \votes ->
        not $ AMap.null votes || any AMap.null (AMap.elems votes)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes = votes
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH6NoSelfOutput :: ValidArgs -> Property
invariantRTH6NoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= rotateScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == rotateScriptAddress = TxOut{txOutAddress = newAddress, ..}
          | otherwise = TxOut{..}
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs =
                      map modifyAddress $
                        txInfoOutputs $
                          scriptContextTxInfo ctx
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH7MultipleSelfOutputs :: ValidArgs -> Property
invariantRTH7MultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = rotateScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH8ValueNotPreserved :: ValidArgs -> Property
invariantRTH8ValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= rotateValue)
    let modifyValue TxOut{..}
          | txOutAddress == rotateScriptAddress = TxOut{txOutValue = newValue, ..}
          | otherwise = TxOut{..}
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs =
                      map modifyValue $
                        txInfoOutputs $
                          scriptContextTxInfo ctx
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

invariantRTH9EmptyVotingOutput :: ValidArgs -> Property
invariantRTH9EmptyVotingOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT _ datum redeemer ctx -> do
    let newDatum = HotLockDatum []
    let modifyDatum TxOut{..}
          | txOutAddress == rotateScriptAddress =
              TxOut
                { txOutDatum = OutputDatum $ Datum $ toBuiltinData newDatum
                , ..
                }
          | otherwise = TxOut{..}
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs =
                      map modifyDatum $
                        txInfoOutputs $
                          scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldNFT hotNFT rotateHotCredential datum redeemer ctx' === False

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
    f rotateColdNFT rotateHotNFT inColdDatum inDatum RotateHot
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= rotateScriptRef) . txInInfoOutRef)
      additionalRefInputs <-
        listOf $
          arbitrary
            `suchThat` (not . hasToken rotateColdNFT . txInInfoResolved)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential rotateScriptAddress . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      refInputs <- shuffle $ refInput : additionalRefInputs
      outputs <- shuffle $ output : additionalOutputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = rotateExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      authSigners <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      let addedVoters = pubKeyHash <$> filter (not . (`elem` rotateVoting)) outVoters
      signers <- shuffle $ authSigners <> addedVoters
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
      pure $ ScriptContext info $ Spending rotateScriptRef
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
        , [TxInfo{txInfoOutputs = x, ..} | x <- shrinkOutputs txInfoOutputs]
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
    shrinkOutputs [] = []
    shrinkOutputs (output' : ins)
      | output' == output = (output' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink output'
            , (output' :) <$> shrink ins
            , pure ins
            ]
    allSigners = nubBy (on (==) pubKeyHash) $ delegationUsers inColdDatum
    inColdDatum =
      ColdLockDatum
        { certificateAuthority = rotateCA
        , membershipUsers = rotateMembership
        , delegationUsers =
            rotateDelegationPre
              <> (rotateExtraDelegation : rotateDelegationPost)
        }
    inDatum = HotLockDatum rotateVoting
    outVoters = rotateNewVotingPre <> (rotateNewExtraVoting : rotateNewVotingPost)
    outDatum =
      inDatum
        { votingUsers = outVoters
        }
    output =
      TxOut
        rotateScriptAddress
        rotateValue
        (OutputDatum $ Datum $ toBuiltinData outDatum)
        Nothing
    input =
      TxInInfo rotateScriptRef $
        TxOut
          rotateScriptAddress
          rotateValue
          (OutputDatum $ Datum $ toBuiltinData inDatum)
          Nothing
    AssetClass (coldPolicy, coldName) = rotateColdNFT
    refInput =
      TxInInfo rotateColdScriptRef $
        TxOut
          rotateColdScriptAddress
          ( Value $
              AMap.insert coldPolicy (AMap.singleton coldName 1) $
                getValue rotateColdValue
          )
          (OutputDatum $ Datum $ toBuiltinData inColdDatum)
          Nothing

data ValidArgs = ValidArgs
  { rotateScriptRef :: TxOutRef
  , rotateScriptAddress :: Address
  , rotateColdScriptRef :: TxOutRef
  , rotateColdScriptAddress :: Address
  , rotateValue :: Value
  , rotateColdValue :: Value
  , rotateCA :: Identity
  , rotateMembership :: [Identity]
  , rotateDelegationPre :: [Identity]
  , rotateExtraDelegation :: Identity
  , rotateDelegationPost :: [Identity]
  , rotateVoting :: [Identity]
  , rotateNewVotingPre :: [Identity]
  , rotateNewExtraVoting :: Identity
  , rotateNewVotingPost :: [Identity]
  , rotateColdNFT :: AssetClass
  , rotateHotNFT :: AssetClass
  , rotateHotCredential :: HotCommitteeCredential
  , rotateExcessSignatureFraction :: Fraction
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
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure coldNFT
      <*> arbitrary `suchThat` (/= coldNFT)
      <*> arbitrary
      <*> arbitrary
  shrink = filter (not . invalid) . genericShrink
    where
      invalid ValidArgs{..} = rotateColdNFT == rotateHotNFT
