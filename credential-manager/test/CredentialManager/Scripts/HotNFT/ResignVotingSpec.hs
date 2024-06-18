module CredentialManager.Scripts.HotNFT.ResignVotingSpec where

import CredentialManager.Api
import CredentialManager.Gen ()
import CredentialManager.Scripts.HotNFT
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  Address (..),
  CurrencySymbol,
  Datum (..),
  HotCommitteeCredential,
  OutputDatum (..),
  ToData (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  Value,
 )
import qualified PlutusTx.AssocMap as AMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop
    "Invariant RV1: ResignVoting fails if not signed by resignee"
    invariantRV1ResignVotingNotSigned
  prop
    "Invariant RV2: ResignVoting fails if resignee not in delegation group"
    invariantRV2NonVoting
  prop
    "Invariant RV3: ResignVoting fails if resignee present in output"
    invariantRV3NotRemoved
  prop
    "Invariant RV4: ResignVoting fails if extra delegators removed"
    invariantRV4ExtraDelegatorsRemoved
  prop
    "Invariant RV5: ResignVoting fails if last delegator resigns"
    invariantRV5ResignLastDelegator
  prop
    "Invariant RV6: ResignVoting fails if any votes are present"
    invariantRV6ExtraneousCertificates
  prop
    "Invariant RV7: ResignVoting fails without output to self"
    invariantRV7NoSelfOutput
  prop
    "Invariant RV8: ResignVoting fails with multiple outputs to self"
    invariantRV8MultipleSelfOutputs
  prop
    "Invariant RV9: ResignVoting fails if value not preserved"
    invariantRV9ValueNotPreserved
  prop
    "Invariant RV10: ResignVoting fails if self output contains reference script"
    invariantRV10ReferenceScriptInOutput
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx ->
        hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx
          === True

invariantRV1ResignVotingNotSigned :: ValidArgs -> Property
invariantRV1ResignVotingNotSigned args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoSignatories =
                      filter (/= pubKeyHash resignVotingResignee) $
                        txInfoSignatories $
                          scriptContextTxInfo ctx
                  }
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
        === False

invariantRV2NonVoting :: ValidArgs -> Property
invariantRV2NonVoting args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    let delegationGroup = votingUsers datum
    let datum' = datum{votingUsers = filter (/= resignVotingResignee) delegationGroup}
    counterexample ("Datum: " <> show datum') $
      hotNFTScript coldNFT hotNFT resignVotingHotCredential datum' redeemer ctx
        === False

invariantRV3NotRemoved :: ValidArgs -> Property
invariantRV3NotRemoved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    let newDatum =
          HotLockDatum $
            resignVotingPre <> (resignVotingResignee : resignVotingPost)
    let modifyDatum TxOut{..}
          | txOutAddress == resignVotingScriptAddress =
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
      hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
        === False

invariantRV4ExtraDelegatorsRemoved :: ValidArgs -> Property
invariantRV4ExtraDelegatorsRemoved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    let otherDelegators = resignVotingPre <> resignVotingPost
    if null otherDelegators
      then pure discard
      else do
        extraRemoved <- sublistOf otherDelegators `suchThat` (not . null)
        let newDatum =
              HotLockDatum $
                filter (`notElem` extraRemoved) $
                  resignVotingPre <> resignVotingPost
        let modifyDatum TxOut{..}
              | txOutAddress == resignVotingScriptAddress =
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
              hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
                === False

invariantRV5ResignLastDelegator :: ValidArgs -> Property
invariantRV5ResignLastDelegator ValidArgs{..} = do
  let args =
        ValidArgs
          { resignVotingPre = []
          , resignVotingPost = []
          , ..
          }
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx
      === False

invariantRV6ExtraneousCertificates :: ValidArgs -> Property
invariantRV6ExtraneousCertificates args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
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
        hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
          === False

invariantRV7NoSelfOutput :: ValidArgs -> Property
invariantRV7NoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= resignVotingScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == resignVotingScriptAddress =
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
        hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
          === False

invariantRV8MultipleSelfOutputs :: ValidArgs -> Property
invariantRV8MultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = resignVotingScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
          === False

invariantRV9ValueNotPreserved :: ValidArgs -> Property
invariantRV9ValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= resignVotingValue)
    let modifyValue TxOut{..}
          | txOutAddress == resignVotingScriptAddress =
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
        hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
          === False

invariantRV10ReferenceScriptInOutput :: ValidArgs -> Property
invariantRV10ReferenceScriptInOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldNFT hotNFT datum redeemer ctx -> do
    referenceScript <- Just <$> arbitrary
    let addReferenceScript TxOut{..}
          | txOutAddress == resignVotingScriptAddress =
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
        hotNFTScript coldNFT hotNFT resignVotingHotCredential datum redeemer ctx'
          === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> ( AssetClass
       -> AssetClass
       -> HotLockDatum
       -> HotLockRedeemer
       -> ScriptContext
       -> prop
     )
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $
    f resignVotingColdNFT resignVotingHotNFT inDatum $
      ResignVoting resignVotingResignee
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= resignVotingScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential resignVotingScriptAddress . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      extraSigners <- arbitrary `suchThat` notElem resignVotingResignee
      signers <-
        shuffle $ nub $ pubKeyHash <$> resignVotingResignee : extraSigners
      info <-
        TxInfo inputs
          <$> arbitrary
          <*> pure outputs
          <*> arbitrary
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
      pure $ ScriptContext info $ Spending resignVotingScriptRef
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
      HotLockDatum $
        resignVotingPre <> (resignVotingResignee : resignVotingPost)
    outDatum =
      inDatum
        { votingUsers = resignVotingPre <> resignVotingPost
        }
    output =
      TxOut
        resignVotingScriptAddress
        resignVotingValue
        (OutputDatum $ Datum $ toBuiltinData outDatum)
        Nothing
    input =
      TxInInfo resignVotingScriptRef $
        TxOut
          resignVotingScriptAddress
          resignVotingValue
          (OutputDatum $ Datum $ toBuiltinData inDatum)
          Nothing

data ValidArgs = ValidArgs
  { resignVotingScriptRef :: TxOutRef
  , resignVotingScriptAddress :: Address
  , resignVotingHotNFT :: AssetClass
  , resignVotingColdNFT :: AssetClass
  , resignVotingHotCredential :: HotCommitteeCredential
  , resignVotingValue :: Value
  , resignVotingResignee :: Identity
  , resignVotingPre :: [Identity]
  , resignVotingPost :: [Identity]
  }
  deriving (Show, Eq, Generic)

instance Arbitrary ValidArgs where
  arbitrary = do
    (membershipPre, membershipPost) <-
      oneof
        [ (,) <$> listOf1 arbitrary <*> arbitrary
        , (,) <$> arbitrary <*> listOf1 arbitrary
        ]
    hotNFT <- arbitrary
    ValidArgs
      <$> arbitrary
      <*> arbitrary
      <*> pure hotNFT
      <*> arbitrary `suchThat` (/= hotNFT)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary `suchThat` (`notElem` membershipPre <> membershipPost)
      <*> pure membershipPre
      <*> pure membershipPost
  shrink = filter (not . invalid) . genericShrink
    where
      invalid ValidArgs{..} =
        null otherDelegators
          || resignVotingResignee `elem` otherDelegators
          || resignVotingHotNFT == resignVotingColdNFT
        where
          otherDelegators = resignVotingPre <> resignVotingPost
