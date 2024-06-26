module CredentialManager.Scripts.ColdNFTSpec where

import Control.Exception (evaluate)
import CredentialManager.Api
import CredentialManager.Gen (mkValue)
import CredentialManager.Scripts.ColdNFT
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.IO (catchAny, unsafePerformIO)
import PlutusLedgerApi.V1.Value (AssetClass (..), assetClassValueOf)
import PlutusLedgerApi.V3 (
  Address (..),
  ColdCommitteeCredential,
  Credential (..),
  Datum (..),
  FromData (..),
  OutputDatum (..),
  PubKeyHash,
  Redeemer (..),
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  ToData (..),
  TxCert (TxCertAuthHotCommittee, TxCertResignColdCommittee),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  Value,
 )
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
          wrapColdNFTScript coldNFT coldCredential
  prop "Invariant 1: Fails if not spending" invariant1BadPurpose
  prop
    "Invariant 2: Valid transitions preserve address"
    invariant2AddressPreservation
  prop "Invariant 3: Valid transitions preserve value" invariant3ValuePreservation
  prop
    "Invariant 4: Valid transitions don't leave empty membership"
    invariant4MembershipNonempty
  prop
    "Invariant 5: Valid transitions don't leave empty delegation"
    invariant5DelegationNonempty
  prop
    "Invariant 6: Valid transitions don't change the CA"
    invariant6CAPreservation

invariant1BadPurpose :: ScriptArgs -> Property
invariant1BadPurpose args@ScriptArgs{..} =
  forAllScriptContexts False args \ctx ->
    classify (wrapColdNFTScript coldNFT coldCredential ctx) "Valid"
      . label case redeemer of
        AuthorizeHot _ -> "AuthorizeHot"
        ResignCold -> "ResignCold"
        ResignDelegation _ -> "ResignDelegation"
        ResignMembership _ -> "ResignMembership"
        RotateCold -> "RotateCold"
        BurnCold -> "BurnCold"
        UpgradeCold _ -> "UpgradeCold"
      $ forAllShrink genNonSpending shrink \scriptInfo ->
        let ctx' = ctx{scriptContextScriptInfo = scriptInfo}
         in wrapColdNFTScript coldNFT coldCredential ctx' === False

invariant2AddressPreservation :: ScriptArgs -> Property
invariant2AddressPreservation args@ScriptArgs{..} =
  forAllValidTransitions args \output _ ->
    on (===) txOutAddress (txInInfoResolved scriptInput) output

invariant3ValuePreservation :: ScriptArgs -> Property
invariant3ValuePreservation args@ScriptArgs{..} =
  forAllValidTransitions args \output _ ->
    on (===) txOutValue (txInInfoResolved scriptInput) output

invariant4MembershipNonempty :: ScriptArgs -> Property
invariant4MembershipNonempty args =
  not (null $ membershipUsers $ datum args) ==>
    forAllValidTransitions args \_ -> not . null . membershipUsers

invariant5DelegationNonempty :: ScriptArgs -> Property
invariant5DelegationNonempty args =
  not (null $ delegationUsers $ datum args) ==>
    forAllValidTransitions args \_ -> not . null . delegationUsers

invariant6CAPreservation :: ScriptArgs -> Property
invariant6CAPreservation args@ScriptArgs{..} =
  forAllValidTransitions args \_ -> on (===) certificateAuthority datum

forAllValidTransitions
  :: (Testable prop) => ScriptArgs -> (TxOut -> ColdLockDatum -> prop) -> Property
forAllValidTransitions args@ScriptArgs{..} f =
  forAllScriptContexts False args \ctx ->
    wrapColdNFTScript coldNFT coldCredential ctx ==>
      case redeemer of
        BurnCold -> discard -- burn is not a transition - it is a terminus
        UpgradeCold _ -> discard -- upgrade is not a transition - it is a terminus
        _ -> case scriptOutput of
          Nothing -> property failed -- All other actions require an output
          Just output ->
            case txOutDatum output of
              OutputDatum (Datum outDatum) -> case fromBuiltinData outDatum of
                Just outDatum' -> property $ f output outDatum'
                Nothing -> property failed
              _ -> property failed

wrapColdNFTScript
  :: AssetClass
  -> ColdCommitteeCredential
  -> ScriptContext
  -> Bool
wrapColdNFTScript nft c ctx =
  unsafePerformIO $
    evaluate (coldNFTScript nft c ctx) `catchAny` const (pure False)

genNonSpending :: Gen ScriptInfo
genNonSpending =
  oneof
    [ MintingScript <$> arbitrary
    , RewardingScript <$> arbitrary
    , CertifyingScript <$> arbitrary <*> arbitrary
    , VotingScript <$> arbitrary
    , ProposingScript <$> arbitrary <*> arbitrary
    ]

nonDelegationSigners :: ColdLockDatum -> Gen [PubKeyHash]
nonDelegationSigners ColdLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash delegationUsers))

nonMembershipSigners :: ColdLockDatum -> Gen [PubKeyHash]
nonMembershipSigners ColdLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash membershipUsers))

data ScriptArgs = ScriptArgs
  { coldNFT :: AssetClass
  , coldCredential :: ColdCommitteeCredential
  , datum :: ColdLockDatum
  , redeemer :: ColdLockRedeemer
  , scriptInput :: TxInInfo
  , scriptOutput :: Maybe TxOut
  , signatories :: [PubKeyHash]
  , certs :: [TxCert]
  }
  deriving (Eq, Show, Generic)

instance Arbitrary ScriptArgs where
  arbitrary = importanceSampleScriptArgs False
  shrink = genericShrink

importanceSampleScriptArgs :: Bool -> Gen ScriptArgs
importanceSampleScriptArgs onlyValid = do
  coldNFT <- arbitrary
  coldCredential <- arbitrary
  redeemer <- importanceSampleRedeemers
  (inDatum, mOutDatum) <- importanceSampleData onlyValid redeemer
  scriptInput <-
    TxInInfo <$> arbitrary <*> importanceSampleScriptInput onlyValid coldNFT inDatum
  scriptOutput <-
    traverse
      (importanceSampleScriptOutput onlyValid (txInInfoResolved scriptInput))
      mOutDatum
  certs <- importanceSampleCerts onlyValid coldCredential redeemer
  signatories <- importanceSampleSigners onlyValid inDatum mOutDatum redeemer
  let datum = inDatum
  pure ScriptArgs{..}

importanceSampleRedeemers :: Gen ColdLockRedeemer
importanceSampleRedeemers =
  frequency
    [ (5, AuthorizeHot <$> arbitrary)
    , (2, pure ResignCold)
    , (5, ResignDelegation <$> arbitrary)
    , (2, pure RotateCold)
    , (1, pure BurnCold)
    , (1, UpgradeCold <$> arbitrary)
    ]

importanceSampleData
  :: Bool -> ColdLockRedeemer -> Gen (ColdLockDatum, Maybe ColdLockDatum)
importanceSampleData onlyValid redeemer = do
  inDatum <-
    ColdLockDatum
      <$> arbitrary
      <*> importanceSampleMembership onlyValid
      <*> importanceSampleDelegation onlyValid redeemer
  (inDatum,) <$> importanceSampleOutputDatum onlyValid redeemer inDatum

importanceSampleMembership :: Bool -> Gen [Identity]
importanceSampleMembership onlyValid =
  importanceSampleArbitrary onlyValid $
    nubBy (on (==) pubKeyHash) <$> listOf1 arbitrary

importanceSampleDelegation :: Bool -> ColdLockRedeemer -> Gen [Identity]
importanceSampleDelegation onlyValid redeemer =
  importanceSampleArbitrary onlyValid $
    shuffle . nubBy (on (==) pubKeyHash) =<< case redeemer of
      ResignDelegation required ->
        (required :) <$> listOf1 (arbitrary `suchThat` on (/=) pubKeyHash required)
      _ -> listOf1 arbitrary

importanceSampleOutputDatum
  :: Bool -> ColdLockRedeemer -> ColdLockDatum -> Gen (Maybe ColdLockDatum)
importanceSampleOutputDatum onlyValid redeemer inDatum =
  importanceSampleArbitrary onlyValid case redeemer of
    BurnCold -> pure Nothing
    UpgradeCold _ -> pure Nothing
    _ ->
      fmap Just $
        ColdLockDatum
          <$> importanceSampleOutputCA onlyValid inDatum
          <*> importanceSampleOutputMembership onlyValid redeemer inDatum
          <*> importanceSampleOutputDelegation onlyValid redeemer inDatum

importanceSampleOutputCA :: Bool -> ColdLockDatum -> Gen Identity
importanceSampleOutputCA onlyValid =
  importanceSampleArbitrary onlyValid . pure . certificateAuthority

importanceSampleOutputMembership
  :: Bool -> ColdLockRedeemer -> ColdLockDatum -> Gen [Identity]
importanceSampleOutputMembership onlyValid redeemer ColdLockDatum{..} =
  importanceSampleArbitrary onlyValid case redeemer of
    AuthorizeHot _ -> pure membershipUsers
    ResignCold -> pure membershipUsers
    ResignDelegation _ -> pure membershipUsers
    ResignMembership user -> pure $ filter (/= user) membershipUsers
    RotateCold -> nubBy (on (==) pubKeyHash) <$> listOf1 arbitrary
    BurnCold -> arbitrary
    UpgradeCold _ -> arbitrary

importanceSampleOutputDelegation
  :: Bool -> ColdLockRedeemer -> ColdLockDatum -> Gen [Identity]
importanceSampleOutputDelegation onlyValid redeemer ColdLockDatum{..} =
  importanceSampleArbitrary onlyValid case redeemer of
    AuthorizeHot _ -> pure delegationUsers
    ResignCold -> pure delegationUsers
    ResignDelegation user -> pure $ filter (/= user) delegationUsers
    ResignMembership _ -> pure delegationUsers
    RotateCold -> nubBy (on (==) pubKeyHash) <$> listOf1 arbitrary
    BurnCold -> arbitrary
    UpgradeCold _ -> arbitrary

forAllScriptContexts
  :: (Testable prop) => Bool -> ScriptArgs -> (ScriptContext -> prop) -> Property
forAllScriptContexts onlyValid ScriptArgs{..} = forAllShrink gen shrink'
  where
    gen = do
      let scriptInputRef = txInInfoOutRef scriptInput
      let scriptCredential = addressCredential $ txOutAddress $ txInInfoResolved scriptInput
      extraInputs <-
        nubBy (on (==) txInInfoOutRef)
          <$> arbitrary `suchThat` (notElem scriptInputRef . fmap txInInfoOutRef)
      inputs <- shuffle $ scriptInput : extraInputs
      outputs <- case scriptOutput of
        Nothing -> case redeemer of
          BurnCold ->
            listOf $
              arbitrary `suchThat` \TxOut{..} ->
                addressCredential txOutAddress /= scriptCredential
                  && assetClassValueOf txOutValue coldNFT == 0
          UpgradeCold destination -> do
            upgradeOut <-
              importanceSampleUpgradeDestinationOutput onlyValid destination coldNFT
            extraOutputs <-
              listOf $
                arbitrary `suchThat` \TxOut{..} ->
                  addressCredential txOutAddress /= scriptCredential
                    && assetClassValueOf txOutValue coldNFT == 0
            shuffle $ upgradeOut : extraOutputs
          _ -> pure []
        Just output -> do
          extraOutputs <- importanceSampleExtraOutputs onlyValid output
          shuffle $ output : extraOutputs
      txInfo <-
        TxInfo inputs
          <$> arbitrary
          <*> pure outputs
          <*> arbitrary
          <*> arbitrary
          <*> pure certs
          <*> arbitrary
          <*> arbitrary
          <*> pure signatories
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
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

importanceSampleUpgradeDestinationOutput
  :: Bool -> ScriptHash -> AssetClass -> Gen TxOut
importanceSampleUpgradeDestinationOutput onlyValid scriptHash nft = do
  addr <- Address (ScriptCredential scriptHash) <$> arbitrary
  TxOut addr
    <$> importanceSampleScriptValue onlyValid nft
    <*> arbitrary
    <*> arbitrary

importanceSampleScriptValue :: Bool -> AssetClass -> Gen Value
importanceSampleScriptValue onlyValid (AssetClass (policy, name)) =
  importanceSampleArbitrary onlyValid do
    policyTokens <- Map.insert name 1 <$> arbitrary
    mkValue <$> arbitrary <*> (Map.insert policy policyTokens <$> arbitrary)

importanceSampleCerts
  :: Bool -> ColdCommitteeCredential -> ColdLockRedeemer -> Gen [TxCert]
importanceSampleCerts onlyValid coldCred =
  importanceSampleArbitrary onlyValid . \case
    AuthorizeHot hotCred -> pure $ pure $ TxCertAuthHotCommittee coldCred hotCred
    ResignCold -> pure $ pure $ TxCertResignColdCommittee coldCred
    _ -> pure []

importanceSampleSigners
  :: Bool
  -> ColdLockDatum
  -> Maybe ColdLockDatum
  -> ColdLockRedeemer
  -> Gen [PubKeyHash]
importanceSampleSigners onlyValid ColdLockDatum{..} outDatum =
  importanceSampleArbitrary onlyValid . \case
    AuthorizeHot _ -> sampleSignersGroup delegationUsers
    ResignCold -> sampleSignersGroup membershipUsers
    ResignDelegation Identity{..} -> pure [pubKeyHash]
    ResignMembership Identity{..} -> pure [pubKeyHash]
    RotateCold -> do
      multisigSigners <- sampleSignersGroup membershipUsers
      pure $
        multisigSigners <> case outDatum of
          Nothing -> []
          Just (ColdLockDatum _ outMembership outDelegation) ->
            (pubKeyHash <$> filter (not . (`elem` membershipUsers)) outMembership)
              <> (pubKeyHash <$> filter (not . (`elem` delegationUsers)) outDelegation)
    BurnCold -> sampleSignersGroup membershipUsers
    UpgradeCold _ -> sampleSignersGroup membershipUsers

sampleSignersGroup :: [Identity] -> Gen [PubKeyHash]
sampleSignersGroup group = do
  let allSigners = nub $ pubKeyHash <$> group
  let minSigners = succ (length allSigners) `div` 2
  signerCount <- chooseInt (minSigners, length allSigners)
  shuffle $ take signerCount allSigners

importanceSampleExtraOutputs :: Bool -> TxOut -> Gen [TxOut]
importanceSampleExtraOutputs onlyValid TxOut{..} =
  importanceSampleArbitrary onlyValid $
    listOf $
      TxOut
        <$> arbitrary `suchThat` on (/=) addressCredential txOutAddress
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

importanceSampleScriptInput :: Bool -> AssetClass -> ColdLockDatum -> Gen TxOut
importanceSampleScriptInput onlyValid nft inDatum =
  TxOut
    <$> (Address . ScriptCredential <$> arbitrary <*> arbitrary)
    <*> importanceSampleScriptValue onlyValid nft
    <*> importanceSampleArbitrary
      onlyValid
      (pure $ OutputDatum $ Datum $ toBuiltinData inDatum)
    <*> arbitrary

importanceSampleScriptOutput :: Bool -> TxOut -> ColdLockDatum -> Gen TxOut
importanceSampleScriptOutput onlyValid TxOut{..} outDatum =
  TxOut
    <$> importanceSampleArbitrary onlyValid (pure txOutAddress)
    <*> importanceSampleArbitrary onlyValid (pure txOutValue)
    <*> importanceSampleArbitrary
      onlyValid
      (pure $ OutputDatum $ Datum $ toBuiltinData outDatum)
    <*> arbitrary

importanceSampleArbitrary :: (Arbitrary a) => Bool -> Gen a -> Gen a
importanceSampleArbitrary True important = important
importanceSampleArbitrary False important =
  frequency
    -- High probability valid/important case
    [ (32, important)
    , -- Low probability stochastic case
      (1, arbitrary)
    ]
