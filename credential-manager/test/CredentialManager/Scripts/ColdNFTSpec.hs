module CredentialManager.Scripts.ColdNFTSpec where

import CredentialManager.Api
import CredentialManager.Gen (Fraction (..))
import CredentialManager.Scripts.ColdNFT
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
  PubKeyHash,
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
  prop "Invariant 1: Fails if not spending" invariant1BadPurpose
  describe "AuthorizeHot" do
    prop
      "Invariant 2: Authorize fails if signed by minority of delegation group"
      invariant2AuthorizeDelegatorMinority
    prop
      "Invariant 3: Authorize ignores duplicate signers in delegation group"
      invariant3DuplicateDelegation
    prop
      "Invariant 4: Authorize fails if cold credential doesn't match"
      invariant4ColdCredentialMismatch
    prop
      "Invariant 5: Authorize fails if hot credential doesn't match"
      invariant5HotCredentialMismatch
    prop
      "Invariant 6: Authorize fails if delegation list is empty"
      invariant6EmptyDelegation
    prop
      "Invariant 7: Authorize fails if extra certs are present"
      invariant7AuthorizeExtraCertificates
    prop
      "Invariant 8: Authorize fails if no certs are present"
      invariant8AuthorizeNoCertificates
    prop
      "Invariant 9: Authorize fails without output to self"
      invariant9AuthorizeNoSelfOutput
    prop
      "Invariant 10: Authorize fails with multiple outputs to self"
      invariant10AuthorizeMultipleSelfOutputs
    prop
      "Invariant 11: Authorize fails if value not preserved"
      invariant11AuthorizeValueNotPreserved
    prop
      "Invariant 12: Authorize fails if datum not preserved"
      invariant12AuthorizeDatumNotPreserved
    prop
      "Invariant 13: Authorize fails if self output contains reference script"
      invariant13AuthorizeReferenceScriptInOutput
  describe "ValidAuthorizeArgs" do
    prop "alwaysValid" \args@ValidAuthorizeArgs{..} ->
      forAllValidScriptContexts args \datum redeemer ctx ->
        coldNFTScript authorizeColdCredential datum redeemer ctx === True

invariant1BadPurpose
  :: ColdCommitteeCredential
  -> ColdLockDatum
  -> ColdLockRedeemer
  -> ScriptContext
  -> Property
invariant1BadPurpose coldCredential datum redeemer ctx = case scriptContextPurpose ctx of
  Spending{} -> discard
  _ -> coldNFTScript coldCredential datum redeemer ctx === False

invariant2AuthorizeDelegatorMinority :: ValidAuthorizeArgs -> Property
invariant2AuthorizeDelegatorMinority args@ValidAuthorizeArgs{..} =
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

invariant3DuplicateDelegation :: ValidAuthorizeArgs -> Property
invariant3DuplicateDelegation args@ValidAuthorizeArgs{..} =
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

invariant4ColdCredentialMismatch :: ValidAuthorizeArgs -> Property
invariant4ColdCredentialMismatch args@ValidAuthorizeArgs{..} =
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

invariant5HotCredentialMismatch :: ValidAuthorizeArgs -> Property
invariant5HotCredentialMismatch args@ValidAuthorizeArgs{..} =
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

invariant6EmptyDelegation :: ValidAuthorizeArgs -> Property
invariant6EmptyDelegation args@ValidAuthorizeArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let datum' = datum{delegationUsers = []}
    coldNFTScript authorizeColdCredential datum' redeemer ctx === False

invariant7AuthorizeExtraCertificates :: ValidAuthorizeArgs -> Property
invariant7AuthorizeExtraCertificates args@ValidAuthorizeArgs{..} =
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

invariant8AuthorizeNoCertificates :: ValidAuthorizeArgs -> Property
invariant8AuthorizeNoCertificates args@ValidAuthorizeArgs{..} =
  forAllValidScriptContexts args \datum redeemer ctx -> do
    let ctx' =
          ctx
            { scriptContextTxInfo =
                (scriptContextTxInfo ctx){txInfoTxCerts = []}
            }
    counterexample ("Context: " <> show ctx') $
      coldNFTScript authorizeColdCredential datum redeemer ctx' === False

invariant9AuthorizeNoSelfOutput :: ValidAuthorizeArgs -> Property
invariant9AuthorizeNoSelfOutput args@ValidAuthorizeArgs{..} =
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

invariant10AuthorizeMultipleSelfOutputs :: ValidAuthorizeArgs -> Property
invariant10AuthorizeMultipleSelfOutputs args@ValidAuthorizeArgs{..} =
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

invariant11AuthorizeValueNotPreserved :: ValidAuthorizeArgs -> Property
invariant11AuthorizeValueNotPreserved args@ValidAuthorizeArgs{..} =
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

invariant12AuthorizeDatumNotPreserved :: ValidAuthorizeArgs -> Property
invariant12AuthorizeDatumNotPreserved args@ValidAuthorizeArgs{..} =
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

invariant13AuthorizeReferenceScriptInOutput :: ValidAuthorizeArgs -> Property
invariant13AuthorizeReferenceScriptInOutput args@ValidAuthorizeArgs{..} =
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

nonDelegationSigners :: ColdLockDatum -> Gen [PubKeyHash]
nonDelegationSigners ColdLockDatum{..} =
  fmap nub $
    arbitrary `suchThat` (not . any (`elem` fmap pubKeyHash delegationUsers))

forAllValidScriptContexts
  :: (Testable prop)
  => ValidAuthorizeArgs
  -> (ColdLockDatum -> ColdLockRedeemer -> ScriptContext -> prop)
  -> Property
forAllValidScriptContexts ValidAuthorizeArgs{..} f =
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

data ValidAuthorizeArgs = ValidAuthorizeArgs
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

instance Arbitrary ValidAuthorizeArgs where
  arbitrary =
    ValidAuthorizeArgs
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink
