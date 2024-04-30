module CredentialManager.Scripts.HotNFT.UnlockHotSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFTSpec (nonDelegationSigners)
import CredentialManager.Scripts.HotNFT
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  Address,
  CurrencySymbol,
  Datum (..),
  HotCommitteeCredential,
  OutputDatum (..),
  ToData (..),
  TokenName,
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
    "Invariant UH1: UnlockHot fails if signed by minority of delegation group"
    invariantUH1UnlockHotDelegationMinority
  prop
    "Invariant UH2: UnlockHot ignores duplicate signers in delegation group"
    invariantUH2DuplicateDelegation
  prop
    "Invariant UH3: UnlockHot fails if delegation list is empty"
    invariantUH3EmptyDelegation
  prop
    "Invariant UH4: RotateHot fails if the cold script reference input is missing"
    invariantUH4ColdRefMissing
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx ->
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx === True

invariantUH1UnlockHotDelegationMinority :: ValidArgs -> Property
invariantUH1UnlockHotDelegationMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy coldDatum datum redeemer ctx -> do
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
            hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH2DuplicateDelegation :: ValidArgs -> Property
invariantUH2DuplicateDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy coldDatum datum redeemer ctx -> do
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
          | AMap.member coldPolicy $ getValue txOutValue =
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
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === True

invariantUH3EmptyDelegation :: ValidArgs -> Property
invariantUH3EmptyDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy coldDatum datum redeemer ctx -> do
    let newDatum = coldDatum{delegationUsers = []}
    let modifyDatum (TxInInfo ref TxOut{..})
          | AMap.member coldPolicy $ getValue txOutValue =
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
      hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH4ColdRefMissing :: ValidArgs -> Property
invariantUH4ColdRefMissing args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoReferenceInputs =
                      filter
                        ( not
                            . AMap.member coldPolicy
                            . getValue
                            . txOutValue
                            . txInInfoResolved
                        )
                        $ txInfoReferenceInputs
                        $ scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH5ExtraneousVotes :: ValidArgs -> Property
invariantUH5ExtraneousVotes args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
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
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH6NoSelfOutput :: ValidArgs -> Property
invariantUH6NoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
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
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH7MultipleSelfOutputs :: ValidArgs -> Property
invariantUH7MultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
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
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH8ValueNotPreserved :: ValidArgs -> Property
invariantUH8ValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
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
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH9EmptyVotingOutput :: ValidArgs -> Property
invariantUH9EmptyVotingOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
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
      hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

invariantUH11ReferenceScriptInOutput :: ValidArgs -> Property
invariantUH11ReferenceScriptInOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy _ datum redeemer ctx -> do
    referenceScript <- Just <$> arbitrary
    let addReferenceScript TxOut{..}
          | txOutAddress == rotateScriptAddress =
              TxOut
                { txOutReferenceScript = referenceScript
                , ..
                }
          | otherwise = TxOut{..}
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs =
                      map addReferenceScript $
                        txInfoOutputs $
                          scriptContextTxInfo ctx
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldPolicy rotateHotCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> ( CurrencySymbol
       -> ColdLockDatum
       -> HotLockDatum
       -> HotLockRedeemer
       -> ScriptContext
       -> prop
     )
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f rotateColdPolicy inColdDatum datum UnlockHot
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= rotateScriptRef) . txInInfoOutRef)
      additionalRefInputs <-
        listOf $
          arbitrary
            `suchThat` ( (not . AMap.member rotateColdPolicy)
                          . getValue
                          . txOutValue
                          . txInInfoResolved
                       )
      outputs <- arbitrary
      inputs <- shuffle $ input : additionalInputs
      refInputs <- shuffle $ refInput : additionalRefInputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = rotateExcessSignatureFraction
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
        { certificateAuthority = rotateCA
        , membershipUsers = rotateMembership
        , delegationUsers =
            rotateDelegationPre
              <> (rotateExtraDelegation : rotateDelegationPost)
        }
    datum = HotLockDatum rotateVoting
    input =
      TxInInfo rotateScriptRef $
        TxOut
          rotateScriptAddress
          rotateValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing
    refInput =
      TxInInfo rotateColdScriptRef $
        TxOut
          rotateColdScriptAddress
          ( Value $
              AMap.insert rotateColdPolicy (AMap.singleton rotateColdToken 1) $
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
  , rotateColdPolicy :: CurrencySymbol
  , rotateColdToken :: TokenName
  , rotateHotCredential :: HotCommitteeCredential
  , rotateExcessSignatureFraction :: Fraction
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
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink
