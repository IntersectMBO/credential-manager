module CredentialManager.Scripts.HotNFTSpec where

import Control.Exception (evaluate)
import CredentialManager.Api
import CredentialManager.Gen (mkValue)
import CredentialManager.Scripts.ColdNFTSpec (
  importanceSampleArbitrary,
  importanceSampleExtraOutputs,
  importanceSampleMembership,
  sampleSignersGroup,
 )
import CredentialManager.Scripts.HotNFT
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.IO (catchAny, unsafePerformIO)
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  Datum (..),
  FromData (..),
  GovernanceActionId,
  HotCommitteeCredential,
  Map,
  OutputDatum (..),
  PubKeyHash,
  ToData (..),
  TxInInfo (..),
  TxOut (..),
  Value (..),
  Voter (..),
 )
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusTx.AssocMap as AMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property (failed)

spec :: Spec
spec = do
  describe "ScriptArgs" do
    prop "onlyValid => valid" $
      forAll (importanceSampleScriptArgs True) \args@ScriptArgs{..} ->
        forAllScriptContexts True args $
          wrapHotNFTScript coldPolicy hotCredential datum redeemer
  prop "Invariant 1: Fails if not spending" invariant1BadPurpose
  prop
    "Invariant 2: Valid transitions preserve address"
    invariant2AddressPreservation
  prop "Invariant 3: Valid transitions preserve value" invariant3ValuePreservation
  prop
    "Invariant 4: Valid transitions don't leave empty voting"
    invariant4VotingNonempty

invariant1BadPurpose :: ScriptArgs -> Property
invariant1BadPurpose args@ScriptArgs{..} =
  forAllScriptContexts False args \ctx ->
    classify (wrapHotNFTScript coldPolicy hotCredential datum redeemer ctx) "Valid"
      . label case redeemer of
        Vote -> "Vote"
        ResignVoting _ -> "ResignVoting"
        RotateHot -> "RotateHot"
        UnlockHot -> "UnlockHot"
      $ forAllShrink genNonSpending shrink \purpose ->
        let ctx' = ctx{scriptContextPurpose = purpose}
         in wrapHotNFTScript coldPolicy hotCredential datum redeemer ctx' === False

invariant2AddressPreservation :: ScriptArgs -> Property
invariant2AddressPreservation args@ScriptArgs{..} =
  forAllValidTransitions args \output _ ->
    on (===) txOutAddress (txInInfoResolved scriptInput) output

invariant3ValuePreservation :: ScriptArgs -> Property
invariant3ValuePreservation args@ScriptArgs{..} =
  forAllValidTransitions args \output _ ->
    on (===) txOutValue (txInInfoResolved scriptInput) output

invariant4VotingNonempty :: ScriptArgs -> Property
invariant4VotingNonempty args =
  not (null $ votingUsers $ datum args) ==>
    forAllValidTransitions args \_ -> not . null . votingUsers

forAllValidTransitions
  :: (Testable prop) => ScriptArgs -> (TxOut -> HotLockDatum -> prop) -> Property
forAllValidTransitions args@ScriptArgs{..} f =
  forAllScriptContexts False args \ctx ->
    wrapHotNFTScript coldPolicy hotCredential datum redeemer ctx ==>
      case redeemer of
        UnlockHot -> discard -- unlock is not a transition - it is a terminus
        _ -> case scriptOutput of
          Nothing -> property failed -- All other actions require an output
          Just output ->
            case txOutDatum output of
              OutputDatum (Datum outDatum) -> case fromBuiltinData outDatum of
                Just outDatum' -> property $ f output outDatum'
                Nothing -> property failed
              _ -> property failed

wrapHotNFTScript
  :: CurrencySymbol
  -> HotCommitteeCredential
  -> HotLockDatum
  -> HotLockRedeemer
  -> ScriptContext
  -> Bool
wrapHotNFTScript p c d r ctx =
  unsafePerformIO $
    evaluate (hotNFTScript p c d r ctx) `catchAny` const (pure False)

genNonSpending :: Gen ScriptPurpose
genNonSpending =
  oneof
    [ Minting <$> arbitrary
    , Rewarding <$> arbitrary
    , Certifying <$> arbitrary <*> arbitrary
    , Voting <$> arbitrary
    , Proposing <$> arbitrary <*> arbitrary
    ]

nonVotingSigners :: HotLockDatum -> Gen [PubKeyHash]
nonVotingSigners HotLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash votingUsers))

data ScriptArgs = ScriptArgs
  { coldPolicy :: CurrencySymbol
  , hotCredential :: HotCommitteeCredential
  , datum :: HotLockDatum
  , redeemer :: HotLockRedeemer
  , coldScriptInput :: Maybe TxInInfo
  , scriptInput :: TxInInfo
  , scriptOutput :: Maybe TxOut
  , signatories :: [PubKeyHash]
  , votes :: Map Voter (Map GovernanceActionId PV3.Vote)
  }
  deriving (Eq, Show, Generic)

instance Arbitrary ScriptArgs where
  arbitrary = importanceSampleScriptArgs False
  shrink = genericShrink

importanceSampleScriptArgs :: Bool -> Gen ScriptArgs
importanceSampleScriptArgs onlyValid = do
  hotCredential <- arbitrary
  coldPolicy <- arbitrary
  redeemer <- importanceSampleRedeemers
  coldDatum <- importanceSampleColdDatum onlyValid redeemer
  (inDatum, mOutDatum) <- importanceSampleData onlyValid redeemer
  scriptInput <-
    TxInInfo <$> arbitrary <*> importanceSampleScriptInput onlyValid inDatum
  scriptOutput <-
    traverse
      (importanceSampleScriptOutput onlyValid (txInInfoResolved scriptInput))
      mOutDatum
  votes <- importanceSampleVotes onlyValid hotCredential redeemer
  delegationUsers <- pure $ foldMap delegationUsers coldDatum
  signatories <-
    importanceSampleSigners onlyValid delegationUsers inDatum redeemer
  coldScriptInput <-
    traverse (importanceSampleColdScriptInput onlyValid coldPolicy) coldDatum
  let datum = inDatum
  pure ScriptArgs{..}

importanceSampleColdScriptInput
  :: Bool -> CurrencySymbol -> ColdLockDatum -> Gen TxInInfo
importanceSampleColdScriptInput onlyValid policyId datum =
  TxInInfo
    <$> arbitrary
    <*> ( TxOut
            <$> arbitrary
            <*> importanceSampleColdScriptInputValue onlyValid policyId
            <*> importanceSampleArbitrary
              onlyValid
              (pure $ OutputDatum $ Datum $ toBuiltinData datum)
            <*> arbitrary
        )

importanceSampleColdScriptInputValue :: Bool -> CurrencySymbol -> Gen PV3.Value
importanceSampleColdScriptInputValue onlyValid policyId =
  mkValue
    <$> arbitrary
    <*> importanceSampleArbitrary onlyValid do
      extras <- arbitrary
      tokenName <- arbitrary
      pure $ Map.insert policyId (Map.singleton tokenName 1) extras

importanceSampleColdDatum
  :: Bool -> HotLockRedeemer -> Gen (Maybe ColdLockDatum)
importanceSampleColdDatum onlyValid = \case
  RotateHot ->
    fmap Just $
      ColdLockDatum
        <$> arbitrary
        <*> arbitrary
        <*> importanceSampleMembership onlyValid
  UnlockHot ->
    fmap Just $
      ColdLockDatum
        <$> arbitrary
        <*> arbitrary
        <*> importanceSampleMembership onlyValid
  _ -> arbitrary

importanceSampleRedeemers :: Gen HotLockRedeemer
importanceSampleRedeemers =
  frequency
    [ (2, pure Vote)
    , (5, ResignVoting <$> arbitrary)
    , (2, pure RotateHot)
    , (1, pure UnlockHot)
    ]

importanceSampleData
  :: Bool -> HotLockRedeemer -> Gen (HotLockDatum, Maybe HotLockDatum)
importanceSampleData onlyValid redeemer = do
  inDatum <- HotLockDatum <$> importanceSampleVoting onlyValid redeemer
  (inDatum,) <$> importanceSampleOutputDatum onlyValid redeemer inDatum

importanceSampleVoting :: Bool -> HotLockRedeemer -> Gen [Identity]
importanceSampleVoting onlyValid redeemer =
  importanceSampleArbitrary onlyValid $
    shuffle . nubBy (on (==) pubKeyHash) =<< case redeemer of
      ResignVoting required ->
        (required :) <$> listOf1 (arbitrary `suchThat` on (/=) pubKeyHash required)
      _ -> listOf1 arbitrary

importanceSampleOutputDatum
  :: Bool
  -> HotLockRedeemer
  -> HotLockDatum
  -> Gen (Maybe HotLockDatum)
importanceSampleOutputDatum onlyValid redeemer inDatum =
  importanceSampleArbitrary onlyValid case redeemer of
    UnlockHot -> pure Nothing
    _ ->
      Just
        . HotLockDatum
        <$> importanceSampleOutputVoting onlyValid redeemer inDatum

importanceSampleOutputVoting
  :: Bool -> HotLockRedeemer -> HotLockDatum -> Gen [Identity]
importanceSampleOutputVoting onlyValid redeemer HotLockDatum{..} =
  importanceSampleArbitrary onlyValid case redeemer of
    Vote -> pure votingUsers
    ResignVoting user -> pure $ filter (/= user) votingUsers
    RotateHot -> nubBy (on (==) pubKeyHash) <$> listOf1 arbitrary
    UnlockHot -> arbitrary

forAllScriptContexts
  :: (Testable prop) => Bool -> ScriptArgs -> (ScriptContext -> prop) -> Property
forAllScriptContexts onlyValid ScriptArgs{..} = forAllShrink gen shrink'
  where
    gen = do
      let scriptInputRef = txInInfoOutRef scriptInput
      extraInputs <-
        nubBy (on (==) txInInfoOutRef)
          <$> listOf (arbitrary `suchThat` ((/= scriptInputRef) . txInInfoOutRef))
      extraRefInputs <-
        nubBy (on (==) txInInfoOutRef)
          <$> listOf
            ( arbitrary
                `suchThat` ( not
                              . AMap.member coldPolicy
                              . getValue
                              . txOutValue
                              . txInInfoResolved
                           )
            )
      inputs <- shuffle $ scriptInput : extraInputs
      refInputs <- maybe pure (fmap shuffle . (:)) coldScriptInput extraRefInputs
      outputs <- case scriptOutput of
        Nothing -> arbitrary
        Just output -> do
          extraOutputs <- importanceSampleExtraOutputs onlyValid output
          shuffle $ output : extraOutputs
      txInfo <-
        TxInfo inputs refInputs outputs
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> pure signatories
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> pure votes
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
      purpose <- importanceSampleArbitrary onlyValid $ pure $ Spending scriptInputRef
      pure $ ScriptContext txInfo purpose
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
      | input' == scriptInput = (input' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink input'
            , (input' :) <$> shrink ins
            , pure ins
            ]
    shrinkOutputs [] = []
    shrinkOutputs (output' : ins)
      | Just output' == scriptOutput = (output' :) <$> shrink ins
      | otherwise =
          fold
            [ (: ins) <$> shrink output'
            , (output' :) <$> shrink ins
            , pure ins
            ]

importanceSampleVotes
  :: Bool
  -> HotCommitteeCredential
  -> HotLockRedeemer
  -> Gen (Map Voter (Map GovernanceActionId PV3.Vote))
importanceSampleVotes onlyValid hotCred =
  importanceSampleArbitrary onlyValid . \case
    Vote ->
      AMap.singleton (CommitteeVoter hotCred)
        <$> arbitrary `suchThat` (not . AMap.null)
    _ -> pure AMap.empty

importanceSampleSigners
  :: Bool -> [Identity] -> HotLockDatum -> HotLockRedeemer -> Gen [PubKeyHash]
importanceSampleSigners onlyValid delegationUsers HotLockDatum{..} =
  importanceSampleArbitrary onlyValid . \case
    Vote -> sampleSignersGroup votingUsers
    ResignVoting Identity{..} -> pure [pubKeyHash]
    RotateHot -> sampleSignersGroup delegationUsers
    UnlockHot -> sampleSignersGroup delegationUsers

importanceSampleScriptInput :: Bool -> HotLockDatum -> Gen TxOut
importanceSampleScriptInput onlyValid inDatum =
  TxOut
    <$> arbitrary
    <*> arbitrary
    <*> pure (OutputDatum $ Datum $ toBuiltinData inDatum)
    <*> importanceSampleArbitrary onlyValid (pure Nothing)

importanceSampleScriptOutput :: Bool -> TxOut -> HotLockDatum -> Gen TxOut
importanceSampleScriptOutput onlyValid TxOut{..} outDatum =
  TxOut
    <$> importanceSampleArbitrary onlyValid (pure txOutAddress)
    <*> importanceSampleArbitrary onlyValid (pure txOutValue)
    <*> ( OutputDatum . Datum . toBuiltinData
            <$> importanceSampleArbitrary onlyValid (pure outDatum)
        )
    <*> importanceSampleArbitrary onlyValid (pure Nothing)
