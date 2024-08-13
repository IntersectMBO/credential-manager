module CredentialManager.Scripts.HotNFTSpec where

import Control.Exception (evaluate)
import CredentialManager.Api
import CredentialManager.Gen (genTxInfoMintForNFTBurn)
import CredentialManager.Scripts.ColdNFTSpec (
  genNonSpending,
  importanceSampleArbitrary,
  importanceSampleExtraOutputs,
  importanceSampleMembership,
  importanceSampleScriptValue,
  importanceSampleUpgradeDestinationOutput,
  sampleSignersGroup,
 )
import CredentialManager.Scripts.HotNFT
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (elemIndex, nub, nubBy)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GHC.IO (catchAny, unsafePerformIO)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClassValue,
  assetClassValueOf,
 )
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  Datum (..),
  FromData (..),
  GovernanceActionId,
  HotCommitteeCredential,
  Map,
  OutputDatum (..),
  PubKeyHash,
  Redeemer (..),
  ScriptContext (..),
  ScriptInfo (..),
  ToData (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  Voter (..),
 )
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusTx.AssocMap as AMap
import PlutusTx.Monoid (Group (inv))
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
          hotNFTScript coldNFT hotNFT hotCredential
  prop "Invariant 1: Fails if not spending" invariant1BadPurpose
  prop
    "Invariant 2: Valid transitions preserve address"
    invariant2AddressPreservation
  prop "Invariant 3: Valid transitions preserve value" invariant3ValuePreservation
  prop
    "Invariant 4: Valid transitions don't leave empty voting"
    invariant4VotingNonempty
  prop
    "Invariant 5: Hot NFT is not present in own input"
    invariant5TokenNotPresentInOwnInput

invariant1BadPurpose :: ScriptArgs -> Property
invariant1BadPurpose args@ScriptArgs{..} =
  forAllScriptContexts False args \ctx ->
    classify
      (hotNFTScript coldNFT hotNFT hotCredential ctx)
      "Valid"
      . label case redeemer of
        Vote -> "Vote"
        ResignVoting _ -> "ResignVoting"
        RotateHot -> "RotateHot"
        BurnHot -> "BurnHot"
        UpgradeHot _ -> "BurnHot"
      $ forAllShrink genNonSpending shrink \scriptInfo ->
        let ctx' = ctx{scriptContextScriptInfo = scriptInfo}
         in hotNFTScript coldNFT hotNFT hotCredential ctx' === False

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

invariant5TokenNotPresentInOwnInput :: ScriptArgs -> Property
invariant5TokenNotPresentInOwnInput args@ScriptArgs{..} =
  forAllScriptContexts True args \ctx ->
    wrapHotNFTScript coldNFT hotNFT hotCredential ctx ==> do
      let genIx len =
            if len == 0
              then pure Nothing
              else
                frequency
                  [ (len - 1, Just <$> choose (0, len - 1))
                  , (1, pure Nothing)
                  ]
          genInputIx = do
            let otherInputsNumber = (length . txInfoInputs . scriptContextTxInfo $ ctx) - 1
            genIx otherInputsNumber
          genOutputIx = do
            let otherOutputsNumber = (length . txInfoOutputs . scriptContextTxInfo $ ctx) - 1
            genIx otherOutputsNumber
          gen = (,) <$> genInputIx <*> genOutputIx

      forAll gen \(possibleNewNFTInputTx, possibleNewNFTOutputTx) -> do
        let ScriptContext{..} = ctx
            TxInfo{txInfoInputs = inputs, txInfoOutputs = outputs} = scriptContextTxInfo
            removeHotNFTFromOutput txOut@(TxOut{txOutValue}) =
              txOut
                { txOutValue =
                    txOutValue <> inv (assetClassValue hotNFT (assetClassValueOf txOutValue hotNFT))
                }
            removeHotNFTFromInput txInInfo@TxInInfo{txInInfoResolved = txOut} = do
              txInInfo{txInInfoResolved = removeHotNFTFromOutput txOut}
            input' = removeHotNFTFromInput scriptInput
            inputs' =
              inputs <&> \input ->
                if input == scriptInput
                  then input'
                  else input
            output' = removeHotNFTFromOutput <$> scriptOutput
            outputs' =
              outputs <&> \output ->
                if Just output == scriptOutput
                  then fromMaybe output output'
                  else output
            addHotNFTToOutput txOut@(TxOut{txOutValue}) =
              txOut{txOutValue = txOutValue <> assetClassValue hotNFT 1}
            addHotNFTToInput txInInfo@TxInInfo{txInInfoResolved = txOut} = do
              txInInfo{txInInfoResolved = addHotNFTToOutput txOut}
            tweakElemSkipping :: forall a. Int -> Int -> [a] -> (a -> a) -> [a]
            tweakElemSkipping ignoredIx targetIx elems f = do
              let targetIx' = if targetIx < ignoredIx then targetIx else targetIx + 1
              flip map (zip [0 ..] elems) \(i, e) ->
                if i == targetIx'
                  then f e
                  else e
            inputs'' = fromMaybe inputs' do
              targetIx <- possibleNewNFTInputTx
              ignoredIx <- elemIndex scriptInput inputs'
              pure $ tweakElemSkipping ignoredIx targetIx inputs' addHotNFTToInput
            outputs'' = fromMaybe outputs' do
              targetIx <- possibleNewNFTOutputTx
              ignoredIx <- output' >>= flip elemIndex outputs'
              pure $ tweakElemSkipping ignoredIx targetIx outputs' addHotNFTToOutput
            ctx' =
              ScriptContext
                { scriptContextTxInfo =
                    scriptContextTxInfo{txInfoInputs = inputs'', txInfoOutputs = outputs''}
                , ..
                }
         in wrapHotNFTScript coldNFT hotNFT hotCredential ctx' === False

wrapHotNFTScript
  :: AssetClass
  -> AssetClass
  -> HotCommitteeCredential
  -> ScriptContext
  -> Bool
wrapHotNFTScript coldNFT hotNFT c ctx =
  unsafePerformIO $
    evaluate (hotNFTScript coldNFT hotNFT c ctx) `catchAny` const (pure False)

forAllValidTransitions
  :: (Testable prop) => ScriptArgs -> (TxOut -> HotLockDatum -> prop) -> Property
forAllValidTransitions args@ScriptArgs{..} f =
  forAllScriptContexts False args \ctx ->
    hotNFTScript coldNFT hotNFT hotCredential ctx ==>
      case redeemer of
        BurnHot -> discard -- unlock is not a transition - it is a terminus
        _ -> case scriptOutput of
          Nothing -> property failed -- All other actions require an output
          Just output ->
            case txOutDatum output of
              OutputDatum (Datum outDatum) -> case fromBuiltinData outDatum of
                Just outDatum' -> property $ f output outDatum'
                Nothing -> property failed
              _ -> property failed

nonVotingSigners :: HotLockDatum -> Gen [PubKeyHash]
nonVotingSigners HotLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash votingUsers))

data ScriptArgs = ScriptArgs
  { coldNFT :: AssetClass
  , hotNFT :: AssetClass
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
  coldNFT <- arbitrary
  hotNFT <- arbitrary `suchThat` if onlyValid then (/= coldNFT) else const True
  redeemer <- importanceSampleRedeemers
  coldDatum <- importanceSampleColdDatum onlyValid redeemer
  (inDatum, mOutDatum) <- importanceSampleData onlyValid redeemer
  scriptInput <-
    TxInInfo <$> arbitrary <*> importanceSampleScriptInput onlyValid hotNFT inDatum
  scriptOutput <-
    traverse
      (importanceSampleScriptOutput onlyValid (txInInfoResolved scriptInput))
      mOutDatum
  votes <- importanceSampleVotes onlyValid hotCredential redeemer
  delegationUsers <- pure $ foldMap delegationUsers coldDatum
  signatories <-
    importanceSampleSigners onlyValid delegationUsers inDatum mOutDatum redeemer
  coldScriptInput <-
    traverse (importanceSampleColdScriptInput onlyValid coldNFT) coldDatum
  let datum = inDatum
  pure ScriptArgs{..}

importanceSampleColdScriptInput
  :: Bool -> AssetClass -> ColdLockDatum -> Gen TxInInfo
importanceSampleColdScriptInput onlyValid coldNFT datum =
  TxInInfo
    <$> arbitrary
    <*> ( TxOut
            <$> arbitrary
            <*> importanceSampleScriptValue onlyValid coldNFT
            <*> importanceSampleArbitrary
              onlyValid
              (pure $ OutputDatum $ Datum $ toBuiltinData datum)
            <*> arbitrary
        )

importanceSampleColdDatum
  :: Bool -> HotLockRedeemer -> Gen (Maybe ColdLockDatum)
importanceSampleColdDatum onlyValid = \case
  RotateHot ->
    fmap Just $
      ColdLockDatum
        <$> arbitrary
        <*> arbitrary
        <*> importanceSampleMembership onlyValid
  BurnHot ->
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
    , (1, pure BurnHot)
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
    BurnHot -> pure Nothing
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
    BurnHot -> arbitrary
    UpgradeHot _ -> arbitrary

forAllScriptContexts
  :: (Testable prop) => Bool -> ScriptArgs -> (ScriptContext -> prop) -> Property
forAllScriptContexts onlyValid ScriptArgs{..} = forAllShrink gen shrink'
  where
    gen = do
      let scriptInputRef = txInInfoOutRef scriptInput
      let scriptCredential = addressCredential $ txOutAddress $ txInInfoResolved scriptInput
      extraInputs <-
        nubBy (on (==) txInInfoOutRef)
          <$> listOf (arbitrary `suchThat` ((/= scriptInputRef) . txInInfoOutRef))
      extraRefInputs <-
        nubBy (on (==) txInInfoOutRef)
          <$> listOf (arbitrary `suchThat` (not . hasToken coldNFT . txInInfoResolved))
      inputs <- shuffle $ scriptInput : extraInputs
      refInputs <- maybe pure (fmap shuffle . (:)) coldScriptInput extraRefInputs
      outputs <- case scriptOutput of
        Nothing -> case redeemer of
          BurnHot ->
            listOf $
              arbitrary `suchThat` \TxOut{..} ->
                addressCredential txOutAddress /= scriptCredential
                  && assetClassValueOf txOutValue hotNFT == 0
          UpgradeHot destination -> do
            upgradeOut <-
              importanceSampleUpgradeDestinationOutput onlyValid destination hotNFT
            extraOutputs <-
              listOf $
                arbitrary `suchThat` \TxOut{..} ->
                  addressCredential txOutAddress /= scriptCredential
                    && assetClassValueOf txOutValue hotNFT == 0
            shuffle $ upgradeOut : extraOutputs
          _ -> pure []
        Just output -> do
          extraOutputs <- importanceSampleExtraOutputs onlyValid output
          shuffle $ output : extraOutputs
      mint <- case redeemer of
        BurnHot -> genTxInfoMintForNFTBurn hotNFT
        _ -> arbitrary

      txInfo <-
        TxInfo inputs refInputs outputs
          <$> arbitrary
          <*> pure mint
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
      scriptInfo <-
        importanceSampleArbitrary onlyValid $
          pure $
            SpendingScript scriptInputRef $
              Just $
                Datum $
                  toBuiltinData datum
      pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) scriptInfo
    shrink' ScriptContext{..} =
      ScriptContext
        <$> shrinkInfo scriptContextTxInfo
        <*> pure scriptContextRedeemer
        <*> pure scriptContextScriptInfo
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

hasToken :: AssetClass -> TxOut -> Bool
hasToken ac TxOut{..} = assetClassValueOf txOutValue ac /= 0

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
  :: Bool
  -> [Identity]
  -> HotLockDatum
  -> Maybe HotLockDatum
  -> HotLockRedeemer
  -> Gen [PubKeyHash]
importanceSampleSigners onlyValid delegationUsers HotLockDatum{..} outDatum =
  importanceSampleArbitrary onlyValid . \case
    Vote -> sampleSignersGroup votingUsers
    ResignVoting Identity{..} -> pure [pubKeyHash]
    RotateHot -> do
      multisigSigners <- sampleSignersGroup delegationUsers
      pure $
        multisigSigners <> case outDatum of
          Nothing -> []
          Just (HotLockDatum outVoters) ->
            pubKeyHash <$> filter (not . (`elem` votingUsers)) outVoters
    BurnHot -> sampleSignersGroup delegationUsers
    UpgradeHot _ -> sampleSignersGroup delegationUsers

importanceSampleScriptInput :: Bool -> AssetClass -> HotLockDatum -> Gen TxOut
importanceSampleScriptInput onlyValid hotNFT inDatum =
  TxOut
    <$> (Address . ScriptCredential <$> arbitrary <*> arbitrary)
    <*> importanceSampleScriptValue onlyValid hotNFT
    <*> importanceSampleArbitrary
      onlyValid
      (pure $ OutputDatum $ Datum $ toBuiltinData inDatum)
    <*> arbitrary

importanceSampleScriptOutput :: Bool -> TxOut -> HotLockDatum -> Gen TxOut
importanceSampleScriptOutput onlyValid TxOut{..} outDatum =
  TxOut
    <$> importanceSampleArbitrary onlyValid (pure txOutAddress)
    <*> importanceSampleArbitrary onlyValid (pure txOutValue)
    <*> importanceSampleArbitrary
      onlyValid
      (pure $ OutputDatum $ Datum $ toBuiltinData outDatum)
    <*> importanceSampleArbitrary
      onlyValid
      (pure txOutReferenceScript)
