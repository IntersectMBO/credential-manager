module CredentialManager.Scripts.ColdNFT.ResignMembershipSpec where

import CredentialManager.Api
import CredentialManager.Gen ()
import CredentialManager.Scripts.ColdNFT
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub)
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
    "Invariant RM1: ResignMembership fails if not signed by resignee"
    invariantRM1ResignMembershipNotSigned
  prop
    "Invariant RM2: ResignMembership fails if resignee not in membership group"
    invariantRM2NonMembership
  prop
    "Invariant RM3: ResignMembership fails if resignee present in output"
    invariantRM3NotRemoved
  prop
    "Invariant RM4: ResignMembership fails if extra delegators removed"
    invariantRM4ExtraDelegatorsRemoved
  prop
    "Invariant RM5: ResignMembership fails if last delegator resigns"
    invariantRM5ResignLastDelegator
  prop
    "Invariant RM6: ResignMembership fails if any certs are present"
    invariantRM6ExtraneousCertificates
  prop
    "Invariant RM7: ResignMembership fails without output to self"
    invariantRM7NoSelfOutput
  prop
    "Invariant RM8: ResignMembership fails with multiple outputs to self"
    invariantRM8MultipleSelfOutputs
  prop
    "Invariant RM9: ResignMembership fails if value not preserved"
    invariantRM9ValueNotPreserved
  prop
    "Invariant RM10: ResignMembership fails if membership users are changed"
    invariantRM10MembershipChanged
  prop
    "Invariant RM11: ResignMembership fails if CA is changed"
    invariantRM11CAChanged
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx === True

invariantRM1ResignMembershipNotSigned :: ValidArgs -> Property
invariantRM1ResignMembershipNotSigned args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoSignatories =
                      filter (/= pubKeyHash resignMembershipResignee) $
                        txInfoSignatories $
                          scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
        === False

invariantRM2NonMembership :: ValidArgs -> Property
invariantRM2NonMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let membershipGroup = membershipUsers datum
    let datum' = datum{membershipUsers = filter (/= resignMembershipResignee) membershipGroup}
    counterexample ("Datum: " <> show datum') $
      coldNFTScript coldNFT resignMembershipColdCredential datum' redeemer ctx
        === False

invariantRM3NotRemoved :: ValidArgs -> Property
invariantRM3NotRemoved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let newDatum =
          ColdLockDatum
            resignMembershipCA
            ( resignMembershipMembershipPre
                <> (resignMembershipResignee : resignMembershipMembershipPost)
            )
            resignMembershipDelegation
    let modifyDatum TxOut{..}
          | txOutAddress == resignMembershipScriptAddress =
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
      coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
        === False

invariantRM4ExtraDelegatorsRemoved :: ValidArgs -> Property
invariantRM4ExtraDelegatorsRemoved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let otherDelegators = resignMembershipMembershipPre <> resignMembershipMembershipPost
    if null otherDelegators
      then pure discard
      else do
        extraRemoved <- sublistOf otherDelegators `suchThat` (not . null)
        let newDatum =
              ColdLockDatum
                resignMembershipCA
                ( filter (`notElem` extraRemoved) $
                    resignMembershipMembershipPre
                      <> resignMembershipMembershipPost
                )
                resignMembershipDelegation
        let modifyDatum TxOut{..}
              | txOutAddress == resignMembershipScriptAddress =
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
              coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
                === False

invariantRM5ResignLastDelegator :: ValidArgs -> Property
invariantRM5ResignLastDelegator ValidArgs{..} = do
  let args =
        ValidArgs
          { resignMembershipMembershipPre = []
          , resignMembershipMembershipPost = []
          , ..
          }
  forAllValidScriptContexts args \datum redeemer ctx -> do
    coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx
      === False

invariantRM6ExtraneousCertificates :: ValidArgs -> Property
invariantRM6ExtraneousCertificates args@ValidArgs{..} =
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
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
          === False

invariantRM7NoSelfOutput :: ValidArgs -> Property
invariantRM7NoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= resignMembershipScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == resignMembershipScriptAddress =
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
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
          === False

invariantRM8MultipleSelfOutputs :: ValidArgs -> Property
invariantRM8MultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = resignMembershipScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
          === False

invariantRM9ValueNotPreserved :: ValidArgs -> Property
invariantRM9ValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= resignMembershipValue)
    let modifyValue TxOut{..}
          | txOutAddress == resignMembershipScriptAddress =
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
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
          === False

invariantRM10MembershipChanged :: ValidArgs -> Property
invariantRM10MembershipChanged args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newDelegation <- arbitrary `suchThat` (/= resignMembershipDelegation)
    let newDatum =
          ColdLockDatum
            resignMembershipCA
            (resignMembershipMembershipPre <> resignMembershipMembershipPost)
            newDelegation
    let modifyDatum TxOut{..}
          | txOutAddress == resignMembershipScriptAddress =
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
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
          === False

invariantRM11CAChanged :: ValidArgs -> Property
invariantRM11CAChanged args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newCA <- arbitrary `suchThat` (/= resignMembershipCA)
    let newDatum =
          ColdLockDatum
            newCA
            (resignMembershipMembershipPre <> resignMembershipMembershipPost)
            resignMembershipDelegation
    let modifyDatum TxOut{..}
          | txOutAddress == resignMembershipScriptAddress =
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
        coldNFTScript coldNFT resignMembershipColdCredential datum redeemer ctx'
          === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f inDatum $ ResignMembership resignMembershipResignee
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= resignMembershipScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential resignMembershipScriptAddress . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      extraSigners <- arbitrary `suchThat` notElem resignMembershipResignee
      signers <-
        shuffle $ nub $ pubKeyHash <$> resignMembershipResignee : extraSigners
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
      pure $ ScriptContext info $ Spending resignMembershipScriptRef
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
        { certificateAuthority = resignMembershipCA
        , membershipUsers =
            resignMembershipMembershipPre
              <> (resignMembershipResignee : resignMembershipMembershipPost)
        , delegationUsers = resignMembershipDelegation
        }
    outDatum =
      inDatum
        { membershipUsers =
            resignMembershipMembershipPre
              <> resignMembershipMembershipPost
        }
    output =
      TxOut
        resignMembershipScriptAddress
        resignMembershipValue
        (OutputDatum $ Datum $ toBuiltinData outDatum)
        Nothing
    input =
      TxInInfo resignMembershipScriptRef $
        TxOut
          resignMembershipScriptAddress
          resignMembershipValue
          (OutputDatum $ Datum $ toBuiltinData inDatum)
          Nothing

data ValidArgs = ValidArgs
  { coldNFT :: AssetClass
  , resignMembershipScriptRef :: TxOutRef
  , resignMembershipScriptAddress :: Address
  , resignMembershipColdCredential :: ColdCommitteeCredential
  , resignMembershipValue :: Value
  , resignMembershipResignee :: Identity
  , resignMembershipCA :: Identity
  , resignMembershipDelegation :: [Identity]
  , resignMembershipMembershipPre :: [Identity]
  , resignMembershipMembershipPost :: [Identity]
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
          || resignMembershipResignee `elem` otherDelegators
        where
          otherDelegators = resignMembershipMembershipPre <> resignMembershipMembershipPost
