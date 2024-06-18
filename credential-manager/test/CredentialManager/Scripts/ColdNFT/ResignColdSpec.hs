module CredentialManager.Scripts.ColdNFT.ResignColdSpec where

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
  TxCert (..),
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
    "Invariant RC1: ResignCold fails if signed by minority of membership group"
    invariantRC1ResignColdMembershipMinority
  prop
    "Invariant RC2: ResignCold ignores duplicate signers in membership group"
    invariantRC2DuplicateMembership
  prop
    "Invariant RC3: ResignCold fails if cold credential doesn't match"
    invariantRC3ColdCredentialMismatch
  prop
    "Invariant RC4: ResignCold fails if membership list is empty"
    invariantRC4EmptyMembership
  prop
    "Invariant RC5: ResignCold fails if extra certs are present"
    invariantRC5ResignColdExtraCertificates
  prop
    "Invariant RC6: ResignCold fails if no certs are present"
    invariantRC6ResignColdNoCertificates
  prop
    "Invariant RC7: ResignCold fails without output to self"
    invariantRC7ResignColdNoSelfOutput
  prop
    "Invariant RC8: ResignCold fails with multiple outputs to self"
    invariantRC8ResignColdMultipleSelfOutputs
  prop
    "Invariant RC9: ResignCold fails if value not preserved"
    invariantRC9ResignColdValueNotPreserved
  prop
    "Invariant RC10: ResignCold fails if datum not preserved"
    invariantRC10ResignColdDatumNotPreserved
  prop
    "Invariant RC11: ResignCold fails if self output contains reference script"
    invariantRC11ResignColdReferenceScriptInOutput
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx === True

invariantRC1ResignColdMembershipMinority :: ValidArgs -> Property
invariantRC1ResignColdMembershipMinority args@ValidArgs{..} =
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
            coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC2DuplicateMembership :: ValidArgs -> Property
invariantRC2DuplicateMembership args@ValidArgs{..} =
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
        coldNFTScript coldNFT resignColdColdCredential datum' redeemer ctx === True

invariantRC3ColdCredentialMismatch :: ValidArgs -> Property
invariantRC3ColdCredentialMismatch args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    coldCred <- arbitrary `suchThat` (/= resignColdColdCredential)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoTxCerts =
                      [TxCertResignColdCommittee coldCred]
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC4EmptyMembership :: ValidArgs -> Property
invariantRC4EmptyMembership args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let datum' = datum{membershipUsers = []}
    coldNFTScript coldNFT resignColdColdCredential datum' redeemer ctx === False

invariantRC5ResignColdExtraCertificates :: ValidArgs -> Property
invariantRC5ResignColdExtraCertificates args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    extra <- listOf1 arbitrary
    certs <- shuffle $ extra <> txInfoTxCerts (scriptContextTxInfo ctx)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoTxCerts = certs
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC6ResignColdNoCertificates :: ValidArgs -> Property
invariantRC6ResignColdNoCertificates args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoTxCerts = []}
            }
    counterexample ("Context: " <> show ctx') $
      coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC7ResignColdNoSelfOutput :: ValidArgs -> Property
invariantRC7ResignColdNoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= resignColdScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == resignColdScriptAddress = TxOut{txOutAddress = newAddress, ..}
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
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC8ResignColdMultipleSelfOutputs :: ValidArgs -> Property
invariantRC8ResignColdMultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = resignColdScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC9ResignColdValueNotPreserved :: ValidArgs -> Property
invariantRC9ResignColdValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= resignColdValue)
    let modifyValue TxOut{..}
          | txOutAddress == resignColdScriptAddress = TxOut{txOutValue = newValue, ..}
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
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC10ResignColdDatumNotPreserved :: ValidArgs -> Property
invariantRC10ResignColdDatumNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newDatum <-
      oneof
        [ arbitrary `suchThat` (/= toBuiltinData datum)
        , toBuiltinData <$> arbitrary `suchThat` (/= datum)
        ]
    let modifyDatum TxOut{..}
          | txOutAddress == resignColdScriptAddress =
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
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

invariantRC11ResignColdReferenceScriptInOutput :: ValidArgs -> Property
invariantRC11ResignColdReferenceScriptInOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    referenceScript <- Just <$> arbitrary
    let addReferenceScript TxOut{..}
          | txOutAddress == resignColdScriptAddress =
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
        coldNFTScript coldNFT resignColdColdCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f datum' ResignCold
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= resignColdScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $
          arbitrary
            `suchThat` (on (/=) addressCredential resignColdScriptAddress . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = resignColdExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      info <-
        TxInfo inputs
          <$> arbitrary
          <*> pure outputs
          <*> arbitrary
          <*> arbitrary
          <*> pure [TxCertResignColdCommittee resignColdColdCredential]
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
      pure $ ScriptContext info $ Spending resignColdScriptRef
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
    allSigners =
      nubBy (on (==) pubKeyHash) $
        resignColdExtraMembership : membershipUsers resignColdDatum
    datum' = resignColdDatum{membershipUsers = allSigners}
    output =
      TxOut
        resignColdScriptAddress
        resignColdValue
        (OutputDatum $ Datum $ toBuiltinData datum')
        Nothing
    input = TxInInfo resignColdScriptRef output

data ValidArgs = ValidArgs
  { coldNFT :: AssetClass
  , resignColdScriptRef :: TxOutRef
  , resignColdScriptAddress :: Address
  , resignColdValue :: Value
  , resignColdExtraMembership :: Identity
  , resignColdDatum :: ColdLockDatum
  , resignColdColdCredential :: ColdCommitteeCredential
  , resignColdExcessSignatureFraction :: Fraction
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
  shrink = genericShrink
