module CredentialManager.Scripts.HotNFT.VoteSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.HotNFT
import CredentialManager.Scripts.HotNFTSpec (nonVotingSigners)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  Address (..),
  CurrencySymbol,
  Datum (..),
  GovernanceActionId,
  HotCommitteeCredential,
  OutputDatum (..),
  ToData (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  Value,
  Voter (..),
 )
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusTx.AssocMap as AMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop
    "Invariant V1: Vote fails if signed by minority of voting group"
    invariantV1VoteVoterMinority
  prop
    "Invariant V2: Vote ignores duplicate signers in voting group"
    invariantV2DuplicateVoting
  prop
    "Invariant V3: Vote fails if gov action ID match"
    invariantV3GovActionIdMismatch
  prop
    "Invariant V4: Vote fails if the voter doesn't match"
    invariantV4VoterMismatch
  prop
    "Invariant V5: Vote fails if the vote doesn't match"
    invariantV5VoteMismatch
  prop
    "Invariant V6: Vote fails if voting list is empty"
    invariantV6EmptyVoting
  prop
    "Invariant V7: Vote fails if extra votes are present"
    invariantV7ExtraVotes
  prop
    "Invariant V8: Vote fails if no votes are present"
    invariantV8VoteNoVotes
  prop
    "Invariant V10: Vote fails without output to self"
    invariantV9VoteNoSelfOutput
  prop
    "Invariant V10: Vote fails with multiple outputs to self"
    invariantV10VoteMultipleSelfOutputs
  prop
    "Invariant V11: Vote fails if value not preserved"
    invariantV11VoteValueNotPreserved
  prop
    "Invariant V12: Vote fails if datum not preserved"
    invariantV12VoteDatumNotPreserved
  prop
    "Invariant V13: Vote fails if self output contains reference script"
    invariantV13VoteReferenceScriptInOutput
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \coldPolicy datum redeemer ctx ->
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx === True

invariantV1VoteVoterMinority :: ValidArgs -> Property
invariantV1VoteVoterMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    let allSigners = nub $ pubKeyHash <$> votingUsers datum
    let minSigners = succ (length allSigners) `div` 2
    forAllShrink (chooseInt (0, pred minSigners)) shrink \signerCount ->
      forAll (nonVotingSigners datum) \extraSigners -> do
        votingSigners <- take signerCount <$> shuffle allSigners
        signers <- shuffle $ votingSigners <> extraSigners
        let ctx' =
              ctx
                { scriptContextTxInfo =
                    (scriptContextTxInfo ctx)
                      { txInfoSignatories = signers
                      }
                }
        pure $
          counterexample ("Signers: " <> show signers) $
            hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV2DuplicateVoting :: ValidArgs -> Property
invariantV2DuplicateVoting args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    let votingGroup = votingUsers datum
    let maybeChangeCertificateHash user =
          oneof
            [ pure user
            , do x <- arbitrary; pure user{certificateHash = x}
            ]
    duplicate <- traverse maybeChangeCertificateHash =<< sublistOf votingGroup
    votingUsers' <- shuffle $ votingGroup <> duplicate
    let datum' = datum{votingUsers = votingUsers'}
    pure $
      counterexample ("Datum: " <> show datum') $
        hotNFTScript coldPolicy voteHotCredential datum' redeemer ctx === True

invariantV3GovActionIdMismatch :: ValidArgs -> Property
invariantV3GovActionIdMismatch args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    actionId <- arbitrary `suchThat` (/= voteActionId)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes =
                      AMap.singleton (CommitteeVoter voteHotCredential) $
                        AMap.singleton actionId voteVote
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV4VoterMismatch :: ValidArgs -> Property
invariantV4VoterMismatch args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    voter <- arbitrary `suchThat` (/= CommitteeVoter voteHotCredential)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes =
                      AMap.singleton voter $
                        AMap.singleton voteActionId voteVote
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV5VoteMismatch :: ValidArgs -> Property
invariantV5VoteMismatch args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    vote <- arbitrary `suchThat` (/= voteVote)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes =
                      AMap.singleton (CommitteeVoter voteHotCredential) $
                        AMap.singleton voteActionId vote
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV6EmptyVoting :: ValidArgs -> Property
invariantV6EmptyVoting args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    let datum' = datum{votingUsers = []}
    hotNFTScript coldPolicy voteHotCredential datum' redeemer ctx === False

invariantV7ExtraVotes :: ValidArgs -> Property
invariantV7ExtraVotes args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    extra <-
      arbitrary `suchThat` \votes ->
        votes /= txInfoVotes (scriptContextTxInfo ctx)
          && not (AMap.null votes)
          && not (any AMap.null $ AMap.elems votes)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes =
                      AMap.unionWith
                        (AMap.unionWith const)
                        (txInfoVotes (scriptContextTxInfo ctx))
                        extra
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV8VoteNoVotes :: ValidArgs -> Property
invariantV8VoteNoVotes args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoVotes = AMap.empty}
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV9VoteNoSelfOutput :: ValidArgs -> Property
invariantV9VoteNoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= voteScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == voteScriptAddress = TxOut{txOutAddress = newAddress, ..}
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
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV10VoteMultipleSelfOutputs :: ValidArgs -> Property
invariantV10VoteMultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = voteScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV11VoteValueNotPreserved :: ValidArgs -> Property
invariantV11VoteValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= voteValue)
    let modifyValue TxOut{..}
          | txOutAddress == voteScriptAddress = TxOut{txOutValue = newValue, ..}
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
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV12VoteDatumNotPreserved :: ValidArgs -> Property
invariantV12VoteDatumNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    newDatum <-
      oneof
        [ arbitrary `suchThat` (/= toBuiltinData datum)
        , toBuiltinData <$> arbitrary `suchThat` (/= datum)
        ]
    let modifyDatum TxOut{..}
          | txOutAddress == voteScriptAddress =
              TxOut
                { txOutDatum = OutputDatum $ Datum newDatum
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
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

invariantV13VoteReferenceScriptInOutput :: ValidArgs -> Property
invariantV13VoteReferenceScriptInOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \coldPolicy datum redeemer ctx -> do
    referenceScript <- Just <$> arbitrary
    let addReferenceScript TxOut{..}
          | txOutAddress == voteScriptAddress =
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
        hotNFTScript coldPolicy voteHotCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (CurrencySymbol -> HotLockDatum -> HotLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $
    f voteColdPolicy datum $
      Vote voteActionId voteVote
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= voteScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential voteScriptAddress . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = voteExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
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
          <*> pure
            ( AMap.singleton (CommitteeVoter voteHotCredential) $
                AMap.singleton voteActionId voteVote
            )
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
      pure $ ScriptContext info $ Spending voteScriptRef
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
    votingUsers = votePre <> (voteExtra : votePost)
    allSigners = nubBy (on (==) pubKeyHash) votingUsers
    datum = HotLockDatum{..}
    output =
      TxOut
        voteScriptAddress
        voteValue
        (OutputDatum $ Datum $ toBuiltinData datum)
        Nothing
    input = TxInInfo voteScriptRef output

data ValidArgs = ValidArgs
  { voteScriptRef :: TxOutRef
  , voteScriptAddress :: Address
  , voteValue :: Value
  , votePre :: [Identity]
  , voteExtra :: Identity
  , votePost :: [Identity]
  , voteColdPolicy :: CurrencySymbol
  , voteHotCredential :: HotCommitteeCredential
  , voteActionId :: GovernanceActionId
  , voteVote :: PV3.Vote
  , voteExcessSignatureFraction :: Fraction
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
  shrink = genericShrink
