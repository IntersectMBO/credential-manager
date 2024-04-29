module CredentialManager.Scripts.ColdNFT.ResignDelegationSpec where

import CredentialManager.Api
import CredentialManager.Gen ()
import CredentialManager.Scripts.ColdNFT
import Data.Foldable (Foldable (..))
import Data.List (nub)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  Address,
  ColdCommitteeCredential,
  Datum (..),
  OutputDatum (..),
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
    "Invariant RD1: ResignDelegation fails if not signed by resignee"
    invariantRD1ResignDelegationNotSigned
  prop
    "Invariant RD2: ResignDelegation fails if resignee not in delegation group"
    invariantRD2NonDelegation
  prop
    "Invariant RD3: ResignDelegation fails if resignee present in output"
    invariantRD3NotRemoved
  prop
    "Invariant RD4: ResignDelegation fails if extra delegators removed"
    invariantRD4ExtraDelegatorsRemoved
  prop
    "Invariant RD5: ResignDelegation fails if last delegator resigns"
    invariantRD5ResignLastDelegator
  prop
    "Invariant RD6: ResignDelegation fails if any certs are present"
    invariantRD6ExtraneousCertificates
  prop
    "Invariant RD7: ResignDelegation fails without output to self"
    invariantRD7NoSelfOutput
  prop
    "Invariant RD8: ResignDelegation fails with multiple outputs to self"
    invariantRD8MultipleSelfOutputs
  prop
    "Invariant RD9: ResignDelegation fails if value not preserved"
    invariantRD9ValueNotPreserved
  prop
    "Invariant RD10: ResignDelegation fails if membership users are changed"
    invariantRD10MembershipChanged
  prop
    "Invariant RD11: ResignDelegation fails if CA is changed"
    invariantRD11CAChanged
  prop
    "Invariant RD12: ResignDelegation fails if self output contains reference script"
    invariantRD12ReferenceScriptInOutput
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript resignDelegationColdCredential datum redeemer ctx === True

invariantRD1ResignDelegationNotSigned :: ValidArgs -> Property
invariantRD1ResignDelegationNotSigned args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoSignatories =
                      filter (/= pubKeyHash resignDelegationResignee) $
                        txInfoSignatories $
                          scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD2NonDelegation :: ValidArgs -> Property
invariantRD2NonDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let delegationGroup = delegationUsers datum
    let datum' = datum{delegationUsers = filter (/= resignDelegationResignee) delegationGroup}
    counterexample ("Datum: " <> show datum') $
      coldNFTScript resignDelegationColdCredential datum' redeemer ctx === False

invariantRD3NotRemoved :: ValidArgs -> Property
invariantRD3NotRemoved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let newDatum =
          ColdLockDatum
            resignDelegationCA
            resignDelegationMembership
            ( resignDelegationDelegationPre
                <> (resignDelegationResignee : resignDelegationDelegationPost)
            )
    let modifyDatum TxOut{..}
          | txOutAddress == resignDelegationScriptAddress =
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
      coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD4ExtraDelegatorsRemoved :: ValidArgs -> Property
invariantRD4ExtraDelegatorsRemoved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let otherDelegators = resignDelegationDelegationPre <> resignDelegationDelegationPost
    if null otherDelegators
      then pure discard
      else do
        extraRemoved <- sublistOf otherDelegators `suchThat` (not . null)
        let newDatum =
              ColdLockDatum
                resignDelegationCA
                resignDelegationMembership
                ( filter (`notElem` extraRemoved) $
                    resignDelegationDelegationPre
                      <> resignDelegationDelegationPost
                )
        let modifyDatum TxOut{..}
              | txOutAddress == resignDelegationScriptAddress =
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
          counterexample ("ExtraRemoved: " <> show extraRemoved) $
            counterexample ("Context: " <> show ctx') $
              coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD5ResignLastDelegator :: ValidArgs -> Property
invariantRD5ResignLastDelegator ValidArgs{..} = do
  let args =
        ValidArgs
          { resignDelegationDelegationPre = []
          , resignDelegationDelegationPost = []
          , ..
          }
  forAllValidScriptContexts args \datum redeemer ctx -> do
    coldNFTScript resignDelegationColdCredential datum redeemer ctx === False

invariantRD6ExtraneousCertificates :: ValidArgs -> Property
invariantRD6ExtraneousCertificates args@ValidArgs{..} =
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
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD7NoSelfOutput :: ValidArgs -> Property
invariantRD7NoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= resignDelegationScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == resignDelegationScriptAddress =
              TxOut{txOutAddress = newAddress, ..}
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
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD8MultipleSelfOutputs :: ValidArgs -> Property
invariantRD8MultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = resignDelegationScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD9ValueNotPreserved :: ValidArgs -> Property
invariantRD9ValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= resignDelegationValue)
    let modifyValue TxOut{..}
          | txOutAddress == resignDelegationScriptAddress =
              TxOut{txOutValue = newValue, ..}
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
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD10MembershipChanged :: ValidArgs -> Property
invariantRD10MembershipChanged args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newMembership <- arbitrary `suchThat` (/= resignDelegationMembership)
    let newDatum =
          ColdLockDatum
            resignDelegationCA
            newMembership
            (resignDelegationDelegationPre <> resignDelegationDelegationPost)
    let modifyDatum TxOut{..}
          | txOutAddress == resignDelegationScriptAddress =
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
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD11CAChanged :: ValidArgs -> Property
invariantRD11CAChanged args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newCA <- arbitrary `suchThat` (/= resignDelegationCA)
    let newDatum =
          ColdLockDatum
            newCA
            resignDelegationMembership
            (resignDelegationDelegationPre <> resignDelegationDelegationPost)
    let modifyDatum TxOut{..}
          | txOutAddress == resignDelegationScriptAddress =
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
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

invariantRD12ReferenceScriptInOutput :: ValidArgs -> Property
invariantRD12ReferenceScriptInOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    referenceScript <- Just <$> arbitrary
    let addReferenceScript TxOut{..}
          | txOutAddress == resignDelegationScriptAddress =
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
        coldNFTScript resignDelegationColdCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f inDatum $ ResignDelegation resignDelegationResignee
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= resignDelegationScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary `suchThat` ((/= resignDelegationScriptAddress) . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      extraSigners <- arbitrary `suchThat` notElem resignDelegationResignee
      signers <-
        shuffle $ nub $ pubKeyHash <$> resignDelegationResignee : extraSigners
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
      pure $ ScriptContext info $ Spending resignDelegationScriptRef
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
    inDatum =
      ColdLockDatum
        { certificateAuthority = resignDelegationCA
        , membershipUsers = resignDelegationMembership
        , delegationUsers =
            resignDelegationDelegationPre
              <> (resignDelegationResignee : resignDelegationDelegationPost)
        }
    outDatum =
      inDatum
        { delegationUsers =
            resignDelegationDelegationPre
              <> resignDelegationDelegationPost
        }
    output =
      TxOut
        resignDelegationScriptAddress
        resignDelegationValue
        (OutputDatum $ Datum $ toBuiltinData outDatum)
        Nothing
    input =
      TxInInfo resignDelegationScriptRef $
        TxOut
          resignDelegationScriptAddress
          resignDelegationValue
          (OutputDatum $ Datum $ toBuiltinData inDatum)
          Nothing

data ValidArgs = ValidArgs
  { resignDelegationScriptRef :: TxOutRef
  , resignDelegationScriptAddress :: Address
  , resignDelegationColdCredential :: ColdCommitteeCredential
  , resignDelegationValue :: Value
  , resignDelegationResignee :: Identity
  , resignDelegationCA :: Identity
  , resignDelegationMembership :: [Identity]
  , resignDelegationDelegationPre :: [Identity]
  , resignDelegationDelegationPost :: [Identity]
  }
  deriving (Show, Eq, Generic)

instance Arbitrary ValidArgs where
  arbitrary = do
    (membershipPre, membershipPost) <-
      oneof
        [ (,) <$> listOf1 arbitrary <*> arbitrary
        , (,) <$> arbitrary <*> listOf1 arbitrary
        ]
    ValidArgs
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary `suchThat` (`notElem` membershipPre <> membershipPost)
      <*> arbitrary
      <*> arbitrary
      <*> pure membershipPre
      <*> pure membershipPost
  shrink = filter (not . invalid) . genericShrink
    where
      invalid ValidArgs{..} =
        null otherDelegators
          || resignDelegationResignee `elem` otherDelegators
        where
          otherDelegators = resignDelegationDelegationPre <> resignDelegationDelegationPost
