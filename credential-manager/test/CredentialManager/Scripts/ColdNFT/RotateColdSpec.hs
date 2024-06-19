module CredentialManager.Scripts.ColdNFT.RotateColdSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFT
import CredentialManager.Scripts.ColdNFTSpec (nonMembershipSigners)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  Address (..),
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
    "Invariant RTC1: RotateCold fails if signed by minority of membership group"
    invariantRTC1RotateColdMembershipMinority
  prop
    "Invariant RTC2: RotateCold ignores duplicate signers in membership group"
    invariantRTC2DuplicateMembership
  prop
    "Invariant RTC3: RotateCold fails if membership list is empty"
    invariantRTC3EmptyMembership
  prop
    "Invariant RTC4: RotateCold fails if any certs are present"
    invariantRTC4ExtraneousCerts
  prop
    "Invariant RTC5: RotateCold fails without output to self"
    invariantRTC5NoSelfOutput
  prop
    "Invariant RTC6: RotateCold fails with multiple outputs to self"
    invariantRTC6MultipleSelfOutputs
  prop
    "Invariant RTC7: RotateCold fails if value not preserved"
    invariantRTC7ValueNotPreserved
  prop
    "Invariant RTC8: RotateCold fails if CA changed"
    invariantRTC8CaChanged
  prop
    "Invariant RTC9: RotateCold fails if membership empty in output"
    invariantRTC9EmptyMembershipOutput
  prop
    "Invariant RTC10: RotateCold fails if delegation empty in output"
    invariantRTC10EmptyDelegationOutput
  prop
    "Invariant RTC11: RotateCold fails if not signed by added member"
    invariantRTC11AddedNotSignedMembership
  prop
    "Invariant RTC12: RotateCold fails if not signed by added delegate"
    invariantRTC12AddedNotSignedDelegation
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript coldNFT rotateColdCredential datum redeemer ctx === True

invariantRTC1RotateColdMembershipMinority :: ValidArgs -> Property
invariantRTC1RotateColdMembershipMinority args@ValidArgs{..} =
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
            coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC2DuplicateMembership :: ValidArgs -> Property
invariantRTC2DuplicateMembership args@ValidArgs{..} =
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
        coldNFTScript coldNFT rotateColdCredential datum' redeemer ctx === True

invariantRTC3EmptyMembership :: ValidArgs -> Property
invariantRTC3EmptyMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let datum' = datum{membershipUsers = []}
    coldNFTScript coldNFT rotateColdCredential datum' redeemer ctx === False

invariantRTC4ExtraneousCerts :: ValidArgs -> Property
invariantRTC4ExtraneousCerts args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    certs <- listOf1 arbitrary
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoTxCerts = certs
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC5NoSelfOutput :: ValidArgs -> Property
invariantRTC5NoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
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
        coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC6MultipleSelfOutputs :: ValidArgs -> Property
invariantRTC6MultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
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
        coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC7ValueNotPreserved :: ValidArgs -> Property
invariantRTC7ValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
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
        coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC8CaChanged :: ValidArgs -> Property
invariantRTC8CaChanged args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newCA <- arbitrary `suchThat` (/= rotateCA)
    let newDatum =
          ColdLockDatum
            newCA
            ( rotateNewMembershipPre
                <> (rotateNewExtraMembership : rotateNewMembershipPost)
            )
            ( rotateNewDelegationPre
                <> (rotateNewExtraDelegation : rotateNewDelegationPost)
            )
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
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC9EmptyMembershipOutput :: ValidArgs -> Property
invariantRTC9EmptyMembershipOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let newDatum =
          ColdLockDatum
            rotateCA
            []
            ( rotateNewDelegationPre
                <> (rotateNewExtraDelegation : rotateNewDelegationPost)
            )
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
      coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC10EmptyDelegationOutput :: ValidArgs -> Property
invariantRTC10EmptyDelegationOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let newDatum =
          ColdLockDatum
            rotateCA
            ( rotateNewDelegationPre
                <> (rotateNewExtraDelegation : rotateNewDelegationPost)
            )
            []
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
      coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC11AddedNotSignedMembership :: ValidArgs -> Property
invariantRTC11AddedNotSignedMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let inMembers = rotateMembershipPre <> (rotateExtraMembership : rotateMembershipPost)
    let outMembers =
          rotateNewMembershipPre <> (rotateNewExtraMembership : rotateNewMembershipPost)
    let added = pubKeyHash <$> filter (not . (`elem` inMembers)) outMembers
    if null added
      then discard
      else do
        signersRemoved <- sublistOf added `suchThat` (not . null)
        let ctx' =
              ctx
                { scriptContextTxInfo =
                    (scriptContextTxInfo ctx)
                      { txInfoSignatories =
                          filter (not . (`elem` signersRemoved)) $
                            txInfoSignatories $
                              scriptContextTxInfo ctx
                      }
                }
        pure $
          coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

invariantRTC12AddedNotSignedDelegation :: ValidArgs -> Property
invariantRTC12AddedNotSignedDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let inDelegation = rotateDelegation
    let outDelegation =
          rotateNewDelegationPre <> (rotateNewExtraDelegation : rotateNewDelegationPost)
    let added = pubKeyHash <$> filter (not . (`elem` inDelegation)) outDelegation
    if null added
      then discard
      else do
        signersRemoved <- sublistOf added `suchThat` (not . null)
        let ctx' =
              ctx
                { scriptContextTxInfo =
                    (scriptContextTxInfo ctx)
                      { txInfoSignatories =
                          filter (not . (`elem` signersRemoved)) $
                            txInfoSignatories $
                              scriptContextTxInfo ctx
                      }
                }
        pure $
          coldNFTScript coldNFT rotateColdCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f inDatum RotateCold
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= rotateScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential rotateScriptAddress . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = rotateExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      authSigners <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      let addedMembers = pubKeyHash <$> filter (not . (`elem` rotateMembership)) outMembers
      let addedDelegates = pubKeyHash <$> filter (not . (`elem` rotateDelegation)) outDelegates
      signers <- shuffle $ authSigners <> addedMembers <> addedDelegates
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
      pure $ ScriptContext info $ Spending rotateScriptRef
    shrink' ScriptContext{..} =
      ScriptContext
        <$> shrinkInfo scriptContextTxInfo
        <*> pure scriptContextPurpose
    shrinkInfo TxInfo{..} =
      fold
        [ [TxInfo{txInfoInputs = x, ..} | x <- shrinkInputs txInfoInputs]
        , [TxInfo{txInfoReferenceInputs = x, ..} | x <- shrink txInfoReferenceInputs]
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
    shrinkOutputs [] = []
    shrinkOutputs (output' : ins)
      | output' == output = (output' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink output'
            , (output' :) <$> shrink ins
            , pure ins
            ]
    rotateMembership = membershipUsers inDatum
    allSigners = nubBy (on (==) pubKeyHash) rotateMembership
    inDatum =
      ColdLockDatum
        { certificateAuthority = rotateCA
        , membershipUsers =
            rotateMembershipPre
              <> (rotateExtraMembership : rotateMembershipPost)
        , delegationUsers = rotateDelegation
        }
    outMembers =
      rotateNewMembershipPre <> (rotateNewExtraMembership : rotateNewMembershipPost)
    outDelegates =
      rotateNewDelegationPre <> (rotateNewExtraDelegation : rotateNewDelegationPost)
    outDatum =
      inDatum
        { membershipUsers = outMembers
        , delegationUsers = outDelegates
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

data ValidArgs = ValidArgs
  { coldNFT :: AssetClass
  , rotateScriptRef :: TxOutRef
  , rotateScriptAddress :: Address
  , rotateValue :: Value
  , rotateCA :: Identity
  , rotateMembershipPre :: [Identity]
  , rotateExtraMembership :: Identity
  , rotateMembershipPost :: [Identity]
  , rotateDelegation :: [Identity]
  , rotateNewMembershipPre :: [Identity]
  , rotateNewExtraMembership :: Identity
  , rotateNewMembershipPost :: [Identity]
  , rotateNewDelegationPre :: [Identity]
  , rotateNewExtraDelegation :: Identity
  , rotateNewDelegationPost :: [Identity]
  , rotateColdCredential :: ColdCommitteeCredential
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
      <*> arbitrary
  shrink = genericShrink
