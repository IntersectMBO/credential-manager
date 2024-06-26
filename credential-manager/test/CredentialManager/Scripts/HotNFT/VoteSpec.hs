module CredentialManager.Scripts.HotNFT.VoteSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.HotNFT
import CredentialManager.Scripts.HotNFTSpec (nonVotingSigners)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (
  Address (..),
  Datum (..),
  GovernanceActionId,
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
  Value,
  Voter (..),
 )
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusTx.AssocMap as AMap
import qualified PlutusTx.Functor as P
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
    "Invariant V3: Vote fails if own hot credential not present"
    invariantV3HotCredentialMissing
  prop
    "Invariant V4: Vote fails if voting list is empty"
    invariantV4EmptyVoting
  prop
    "Invariant V5: Vote fails if no votes are present"
    invariantV5VoteNoVotes
  prop
    "Invariant V6: Vote fails without output to self"
    invariantV6VoteNoSelfOutput
  prop
    "Invariant V7: Vote fails with multiple outputs to self"
    invariantV7VoteMultipleSelfOutputs
  prop
    "Invariant V8: Vote fails if value not preserved"
    invariantV8VoteValueNotPreserved
  prop
    "Invariant V9: Vote fails if datum not preserved"
    invariantV9VoteDatumNotPreserved
  prop
    "Invariant V11: Vote fails if extraneous votes are present"
    invariantV11ExtraneousVoters
  prop
    "Invariant V12: Vote fails if no votes are cast"
    invariantV12NoVotes
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx ->
        hotNFTScript coldNFT hotNFT voteHotCredential ctx === True

invariantV1VoteVoterMinority :: ValidArgs -> Property
invariantV1VoteVoterMinority args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT datum _ ctx -> do
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
            hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV2DuplicateVoting :: ValidArgs -> Property
invariantV2DuplicateVoting args@ValidArgs{..} =
  forAllValidScriptContexts dupVoter args \coldNFT hotNFT _ _ ctx -> do
    hotNFTScript coldNFT hotNFT voteHotCredential ctx === True
  where
    dupVoter datum = do
      let votingGroup = votingUsers datum
      let maybeChangeCertificateHash user =
            oneof
              [ pure user
              , do x <- arbitrary; pure user{certificateHash = x}
              ]
      duplicate <- traverse maybeChangeCertificateHash =<< sublistOf votingGroup
      votingUsers' <- shuffle $ votingGroup <> duplicate
      pure datum{votingUsers = votingUsers'}

invariantV3HotCredentialMissing :: ValidArgs -> Property
invariantV3HotCredentialMissing args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
    votes <-
      arbitrary
        `suchThat` (not . AMap.member (CommitteeVoter voteHotCredential))
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes = votes
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV4EmptyVoting :: ValidArgs -> Property
invariantV4EmptyVoting args@ValidArgs{..} =
  forAllValidScriptContexts clearVoters args \coldNFT hotNFT _ _ ctx -> do
    hotNFTScript coldNFT hotNFT voteHotCredential ctx === False
  where
    clearVoters datum = pure datum{votingUsers = []}

invariantV5VoteNoVotes :: ValidArgs -> Property
invariantV5VoteNoVotes args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoVotes = AMap.empty}
            }
    counterexample ("Context: " <> show ctx') $
      hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV6VoteNoSelfOutput :: ValidArgs -> Property
invariantV6VoteNoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
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
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV7VoteMultipleSelfOutputs :: ValidArgs -> Property
invariantV7VoteMultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
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
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV8VoteValueNotPreserved :: ValidArgs -> Property
invariantV8VoteValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
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
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV9VoteDatumNotPreserved :: ValidArgs -> Property
invariantV9VoteDatumNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT datum _ ctx -> do
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
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV11ExtraneousVoters :: ValidArgs -> Property
invariantV11ExtraneousVoters args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
    extraneousVotes <-
      arbitrary
        `suchThat` \votes ->
          not (AMap.member (CommitteeVoter voteHotCredential) votes)
            && not (AMap.null votes)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes =
                      AMap.unionWith
                        (const id)
                        (txInfoVotes (scriptContextTxInfo ctx))
                        extraneousVotes
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

invariantV12NoVotes :: ValidArgs -> Property
invariantV12NoVotes args@ValidArgs{..} =
  forAllValidScriptContexts pure args \coldNFT hotNFT _ _ ctx -> do
    votes <-
      P.fmap (const AMap.empty)
        <$> oneof
          [ arbitrary @(AMap.Map _ (AMap.Map GovernanceActionId PV3.Vote))
          , pure $ txInfoVotes $ scriptContextTxInfo ctx
          ]
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoVotes = votes
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        hotNFTScript coldNFT hotNFT voteHotCredential ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => (HotLockDatum -> Gen HotLockDatum)
  -> ValidArgs
  -> ( AssetClass
       -> AssetClass
       -> HotLockDatum
       -> HotLockRedeemer
       -> ScriptContext
       -> prop
     )
  -> Property
forAllValidScriptContexts tweakDatum ValidArgs{..} f =
  forAllShrink gen shrink' \(datum, ctx) ->
    f voteColdNFT voteHotNFT datum Vote ctx
  where
    gen = do
      datum <- tweakDatum HotLockDatum{..}
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= voteScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential voteScriptAddress . txOutAddress)
      inputs <- shuffle $ input datum : additionalInputs
      outputs <- shuffle $ output datum : additionalOutputs
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
          <*> ( AMap.singleton (CommitteeVoter voteHotCredential)
                  <$> arbitrary `suchThat` (not . AMap.null)
              )
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
      let redeemer' = Redeemer $ toBuiltinData Vote
      pure
        ( datum
        , ScriptContext info redeemer' $
            SpendingScript voteScriptRef $
              Just $
                Datum $
                  toBuiltinData datum
        )
    shrink' (datum, ScriptContext{..}) =
      (fmap . fmap) (datum,) . ScriptContext
        <$> shrinkInfo datum scriptContextTxInfo
        <*> pure scriptContextRedeemer
        <*> pure scriptContextScriptInfo
    shrinkInfo datum TxInfo{..} =
      fold
        [ [TxInfo{txInfoInputs = x, ..} | x <- shrinkInputs datum txInfoInputs]
        , [TxInfo{txInfoReferenceInputs = x, ..} | x <- shrink txInfoReferenceInputs]
        , [TxInfo{txInfoOutputs = x, ..} | x <- shrinkOutputs datum txInfoOutputs]
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
    shrinkInputs _ [] = []
    shrinkInputs datum (input' : ins)
      | input' == input datum = (input' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink input'
            , (input' :) <$> shrink ins
            , pure ins
            ]
    shrinkOutputs _ [] = []
    shrinkOutputs datum (output' : ins)
      | output' == output datum = (output' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink output'
            , (output' :) <$> shrink ins
            , pure ins
            ]
    votingUsers = votePre <> (voteExtra : votePost)
    allSigners = nubBy (on (==) pubKeyHash) votingUsers
    output datum =
      TxOut
        voteScriptAddress
        voteValue
        (OutputDatum $ Datum $ toBuiltinData datum)
        Nothing
    input = TxInInfo voteScriptRef . output

data ValidArgs = ValidArgs
  { voteScriptRef :: TxOutRef
  , voteScriptAddress :: Address
  , voteValue :: Value
  , votePre :: [Identity]
  , voteExtra :: Identity
  , votePost :: [Identity]
  , voteColdNFT :: AssetClass
  , voteHotNFT :: AssetClass
  , voteHotCredential :: HotCommitteeCredential
  , voteExcessSignatureFraction :: Fraction
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
      <*> pure coldNFT
      <*> arbitrary `suchThat` (/= coldNFT)
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink
