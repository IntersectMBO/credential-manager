module CredentialManager.Scripts.ColdNFT.BurnColdSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFT
import CredentialManager.Scripts.ColdNFT.RotateColdSpec (updateDatum)
import CredentialManager.Scripts.ColdNFTSpec (nonMembershipSigners)
import CredentialManager.Scripts.HotNFTSpec (hasToken)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValue)
import PlutusLedgerApi.V3 (
  Address,
  ColdCommitteeCredential,
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptInfo (SpendingScript),
  ToData (..),
  TxInInfo (..),
  TxInfo (..),
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
    "Invariant BC1: BurnCold fails if signed by minority of membership group"
    invariantBC1BurnColdMembershipMinority
  prop
    "Invariant BC2: BurnCold ignores duplicate signers in membership group"
    invariantBC2DuplicateMembership
  prop
    "Invariant BC3: BurnCold fails if membership list is empty"
    invariantBC3EmptyMembership
  prop
    "Invariant BC4: BurnCold fails if token not burned"
    invariantBC4NotBurned
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \_ _ ctx ->
        coldNFTScript coldNFT burnColdCredential ctx === True

invariantBC1BurnColdMembershipMinority :: ValidArgs -> Property
invariantBC1BurnColdMembershipMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \datum _ ctx -> do
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
            coldNFTScript coldNFT burnColdCredential ctx' === False

invariantBC2DuplicateMembership :: ValidArgs -> Property
invariantBC2DuplicateMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum _ ctx -> do
    let membershipGroup = membershipUsers datum
    let maybeChangeCertificateHash user =
          oneof
            [ pure user
            , do x <- arbitrary; pure user{certificateHash = x}
            ]
    duplicate <- traverse maybeChangeCertificateHash =<< sublistOf membershipGroup
    membershipUsers' <- shuffle $ membershipGroup <> duplicate
    let datum' = datum{membershipUsers = membershipUsers'}
    let ctx' = updateDatum datum' ctx
    pure $
      counterexample ("Datum: " <> show datum') $
        coldNFTScript coldNFT burnColdCredential ctx' === True

invariantBC3EmptyMembership :: ValidArgs -> Property
invariantBC3EmptyMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum _ ctx -> do
    let datum' = datum{membershipUsers = []}
    let ctx' = updateDatum datum' ctx
    coldNFTScript coldNFT burnColdCredential ctx' === False

invariantBC4NotBurned :: ValidArgs -> Property
invariantBC4NotBurned args@ValidArgs{..} =
  forAllValidScriptContexts args \_ _ ctx -> do
    baseValue <- arbitrary
    let value = baseValue <> assetClassValue coldNFT 1
    output <-
      TxOut
        <$> arbitrary
        <*> pure value
        <*> arbitrary
        <*> arbitrary
    outputs' <- shuffle $ output : txInfoOutputs (scriptContextTxInfo ctx)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoOutputs = outputs'
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT burnColdCredential ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f datum BurnCold
  where
    redeemer = BurnCold
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= burnScriptRef) . txInInfoOutRef)
      inputs <- shuffle $ input : additionalInputs
      outputs <- listOf $ arbitrary `suchThat` (not . hasToken coldNFT)
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = burnExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
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
      let redeemer' = Redeemer $ toBuiltinData redeemer
      pure $
        ScriptContext info redeemer' $
          SpendingScript burnScriptRef $
            Just $
              Datum $
                toBuiltinData datum
    shrink' ScriptContext{..} =
      ScriptContext
        <$> shrinkInfo scriptContextTxInfo
        <*> pure scriptContextRedeemer
        <*> pure scriptContextScriptInfo
    shrinkInfo TxInfo{..} =
      fold
        [ [TxInfo{txInfoInputs = x, ..} | x <- shrinkInputs txInfoInputs]
        , [TxInfo{txInfoReferenceInputs = x, ..} | x <- shrink txInfoReferenceInputs]
        , [TxInfo{txInfoOutputs = x, ..} | x <- shrink txInfoOutputs]
        , [TxInfo{txInfoFee = x, ..} | x <- shrink txInfoFee]
        , [TxInfo{txInfoMint = x, ..} | x <- shrink txInfoMint]
        , [TxInfo{txInfoWdrl = x, ..} | x <- shrink txInfoWdrl]
        , [TxInfo{txInfoValidRange = x, ..} | x <- shrink txInfoValidRange]
        , [TxInfo{txInfoRedeemers = x, ..} | x <- shrink txInfoRedeemers]
        , [TxInfo{txInfoData = x, ..} | x <- shrink txInfoData]
        , [TxInfo{txInfoId = x, ..} | x <- shrink txInfoId]
        , [TxInfo{txInfoVotes = x, ..} | x <- shrink txInfoVotes]
        , [TxInfo{txInfoTxCerts = x, ..} | x <- shrink txInfoTxCerts]
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
    allSigners = nubBy (on (==) pubKeyHash) $ membershipUsers datum
    datum =
      ColdLockDatum
        { certificateAuthority = burnCA
        , membershipUsers =
            burnMembershipPre
              <> (burnExtraMembership : burnMembershipPost)
        , delegationUsers = burnDelegation
        }
    input =
      TxInInfo burnScriptRef $
        TxOut
          burnScriptAddress
          burnValue
          (OutputDatum $ Datum $ toBuiltinData datum)
          Nothing

data ValidArgs = ValidArgs
  { coldNFT :: AssetClass
  , burnScriptRef :: TxOutRef
  , burnScriptAddress :: Address
  , burnValue :: Value
  , burnCA :: Identity
  , burnMembershipPre :: [Identity]
  , burnExtraMembership :: Identity
  , burnMembershipPost :: [Identity]
  , burnDelegation :: [Identity]
  , burnColdCredential :: ColdCommitteeCredential
  , burnExcessSignatureFraction :: Fraction
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
