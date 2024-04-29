module CredentialManager.Scripts.ColdNFT.AuthorizeSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFT
import CredentialManager.Scripts.ColdNFTSpec (nonDelegationSigners)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.List (nub, nubBy)
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  Address,
  ColdCommitteeCredential,
  Datum (..),
  HotCommitteeCredential,
  OutputDatum (..),
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
    "Invariant A1: Authorize fails if signed by minority of delegation group"
    invariantA1AuthorizeDelegatorMinority
  prop
    "Invariant A2: Authorize ignores duplicate signers in delegation group"
    invariantA2DuplicateDelegation
  prop
    "Invariant A3: Authorize fails if cold credential doesn't match"
    invariantA3ColdCredentialMismatch
  prop
    "Invariant A4: Authorize fails if hot credential doesn't match"
    invariantA4HotCredentialMismatch
  prop
    "Invariant A5: Authorize fails if delegation list is empty"
    invariantA5EmptyDelegation
  prop
    "Invariant A6: Authorize fails if extra certs are present"
    invariantA6AuthorizeExtraCertificates
  prop
    "Invariant A7: Authorize fails if no certs are present"
    invariantA7AuthorizeNoCertificates
  prop
    "Invariant A8: Authorize fails without output to self"
    invariantA8AuthorizeNoSelfOutput
  prop
    "Invariant A9: Authorize fails with multiple outputs to self"
    invariantA9AuthorizeMultipleSelfOutputs
  prop
    "Invariant 1A0: Authorize fails if value not preserved"
    invariant1A0AuthorizeValueNotPreserved
  prop
    "Invariant A11: Authorize fails if datum not preserved"
    invariantA11AuthorizeDatumNotPreserved
  prop
    "Invariant A12: Authorize fails if self output contains reference script"
    invariantA12AuthorizeReferenceScriptInOutput
  describe "ValidArgs" do
    prop "alwaysValid" \args@ValidArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript authorizeColdCredential datum redeemer ctx === True

invariantA1AuthorizeDelegatorMinority :: ValidArgs -> Property
invariantA1AuthorizeDelegatorMinority args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let allSigners = nub $ pubKeyHash <$> delegationUsers datum
    let minSigners = succ (length allSigners) `div` 2
    forAllShrink (chooseInt (0, pred minSigners)) shrink \signerCount ->
      forAll (nonDelegationSigners datum) \extraSigners -> do
        delegationSigners <- take signerCount <$> shuffle allSigners
        signers <- shuffle $ delegationSigners <> extraSigners
        let ctx' =
              ctx
                { scriptContextTxInfo =
                    (scriptContextTxInfo ctx)
                      { txInfoSignatories = signers
                      }
                }
        pure $
          counterexample ("Signers: " <> show signers) $
            coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA2DuplicateDelegation :: ValidArgs -> Property
invariantA2DuplicateDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let delegationGroup = delegationUsers datum
    let maybeChangeCertificateHash user =
          oneof
            [ pure user
            , do x <- arbitrary; pure user{certificateHash = x}
            ]
    duplicate <- traverse maybeChangeCertificateHash =<< sublistOf delegationGroup
    delegationUsers' <- shuffle $ delegationGroup <> duplicate
    let datum' = datum{delegationUsers = delegationUsers'}
    pure $
      counterexample ("Datum: " <> show datum') $
        coldNFTScript authorizeColdCredential datum' redeemer ctx === True

invariantA3ColdCredentialMismatch :: ValidArgs -> Property
invariantA3ColdCredentialMismatch args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    coldCred <- arbitrary `suchThat` (/= authorizeColdCredential)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoTxCerts =
                      [TxCertAuthHotCommittee coldCred authorizeHotCredential]
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA4HotCredentialMismatch :: ValidArgs -> Property
invariantA4HotCredentialMismatch args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    hotCred <- arbitrary `suchThat` (/= authorizeHotCredential)
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx)
                  { txInfoTxCerts =
                      [TxCertAuthHotCommittee authorizeColdCredential hotCred]
                  }
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA5EmptyDelegation :: ValidArgs -> Property
invariantA5EmptyDelegation args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let datum' = datum{delegationUsers = []}
    coldNFTScript authorizeColdCredential datum' redeemer ctx === False

invariantA6AuthorizeExtraCertificates :: ValidArgs -> Property
invariantA6AuthorizeExtraCertificates args@ValidArgs{..} =
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
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA7AuthorizeNoCertificates :: ValidArgs -> Property
invariantA7AuthorizeNoCertificates args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoTxCerts = []}
            }
    counterexample ("Context: " <> show ctx') $
      coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA8AuthorizeNoSelfOutput :: ValidArgs -> Property
invariantA8AuthorizeNoSelfOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newAddress <- arbitrary `suchThat` (/= authorizeScriptAddress)
    let modifyAddress TxOut{..}
          | txOutAddress == authorizeScriptAddress = TxOut{txOutAddress = newAddress, ..}
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
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA9AuthorizeMultipleSelfOutputs :: ValidArgs -> Property
invariantA9AuthorizeMultipleSelfOutputs args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let setAddress txOut = txOut{txOutAddress = authorizeScriptAddress}
    newOutputs <- listOf1 $ setAddress <$> arbitrary
    outputs' <- shuffle $ txInfoOutputs (scriptContextTxInfo ctx) <> newOutputs
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoOutputs = outputs'}
            }
    pure $
      counterexample ("Context: " <> show ctx') $
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariant1A0AuthorizeValueNotPreserved :: ValidArgs -> Property
invariant1A0AuthorizeValueNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newValue <- arbitrary `suchThat` (/= authorizeValue)
    let modifyValue TxOut{..}
          | txOutAddress == authorizeScriptAddress = TxOut{txOutValue = newValue, ..}
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
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA11AuthorizeDatumNotPreserved :: ValidArgs -> Property
invariantA11AuthorizeDatumNotPreserved args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    newDatum <-
      oneof
        [ arbitrary `suchThat` (/= toBuiltinData datum)
        , toBuiltinData <$> arbitrary `suchThat` (/= datum)
        ]
    let modifyDatum TxOut{..}
          | txOutAddress == authorizeScriptAddress =
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
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariantA12AuthorizeReferenceScriptInOutput :: ValidArgs -> Property
invariantA12AuthorizeReferenceScriptInOutput args@ValidArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    referenceScript <- Just <$> arbitrary
    let addReferenceScript TxOut{..}
          | txOutAddress == authorizeScriptAddress =
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
        coldNFTScript authorizeColdCredential datum redeemer ctx' === False

forAllValidScriptContexts
  :: (Testable prop)
  => ValidArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidArgs{..} f =
  forAllShrink gen shrink' $ f datum' $ AuthorizeHot authorizeHotCredential
  where
    gen = do
      additionalInputs <-
        listOf $ arbitrary `suchThat` ((/= authorizeScriptRef) . txInInfoOutRef)
      additionalOutputs <-
        listOf $ arbitrary `suchThat` ((/= authorizeScriptAddress) . txOutAddress)
      inputs <- shuffle $ input : additionalInputs
      outputs <- shuffle $ output : additionalOutputs
      let maxSigners = length allSigners
      let minSigners = succ maxSigners `div` 2
      let Fraction excessFraction = authorizeExcessSignatureFraction
      let excessSigners = floor $ fromIntegral (maxSigners - minSigners) * excessFraction
      let signerCount = minSigners + excessSigners
      signers <- fmap pubKeyHash . take signerCount <$> shuffle allSigners
      info <-
        TxInfo inputs
          <$> arbitrary
          <*> pure outputs
          <*> arbitrary
          <*> arbitrary
          <*> pure [TxCertAuthHotCommittee authorizeColdCredential authorizeHotCredential]
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
      pure $ ScriptContext info $ Spending authorizeScriptRef
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
        authorizeExtraDelegation : delegationUsers authorizeDatum
    datum' = authorizeDatum{delegationUsers = allSigners}
    output =
      TxOut
        authorizeScriptAddress
        authorizeValue
        (OutputDatum $ Datum $ toBuiltinData datum')
        Nothing
    input = TxInInfo authorizeScriptRef output

data ValidArgs = ValidArgs
  { authorizeScriptRef :: TxOutRef
  , authorizeScriptAddress :: Address
  , authorizeValue :: Value
  , authorizeExtraDelegation :: Identity
  , authorizeDatum :: ColdLockDatum
  , authorizeColdCredential :: ColdCommitteeCredential
  , authorizeHotCredential :: HotCommitteeCredential
  , authorizeExcessSignatureFraction :: Fraction
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
