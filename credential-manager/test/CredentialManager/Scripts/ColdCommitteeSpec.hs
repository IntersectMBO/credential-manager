module CredentialManager.Scripts.ColdCommitteeSpec where

import CredentialManager.Gen ()
import CredentialManager.Scripts.ColdCommittee
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClassValue,
  assetClassValueOf,
 )
import PlutusLedgerApi.V3 (
  Address,
  OutputDatum,
  Redeemer,
  ScriptContext (..),
  ScriptHash,
  ScriptInfo (..),
  TokenName,
  TxCert,
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef,
  Value (Value, getValue),
 )
import qualified PlutusTx.AssocMap as AMap
import qualified PlutusTx.Prelude as P
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "Invariant 1: Fails if not certifying" invariant1BadPurpose
  prop "Invariant 2: Fails if NFT not spent" invariant2NFTMissing
  prop "Invariant 3: Succeeds if NFT spent" invariant3NFTSpent

invariant1BadPurpose :: AssetClass -> ScriptContext -> Property
invariant1BadPurpose nft ctx = case scriptContextScriptInfo ctx of
  CertifyingScript{} -> discard
  _ -> coldCommitteeScript nft ctx === False

invariant2NFTMissing :: AssetClass -> ScriptContext -> Property
invariant2NFTMissing nft ctx =
  coldCommitteeScript nft ctx' === False
  where
    ctx' =
      ctx
        { scriptContextTxInfo = case ctx of
            ScriptContext{..} ->
              scriptContextTxInfo
                { txInfoInputs = do
                    TxInInfo{..} <- txInfoInputs scriptContextTxInfo
                    let TxOut{..} = txInInfoResolved
                    pure
                      TxInInfo
                        { txInInfoResolved =
                            TxOut
                              { txOutValue =
                                  txOutValue
                                    P.- assetClassValue nft (assetClassValueOf txOutValue nft)
                              , ..
                              }
                        , ..
                        }
                }
        }

invariant3NFTSpent
  :: AssetClass
  -> Redeemer
  -> TxInfo
  -> TxCert
  -> TxOutRef
  -> Address
  -> OutputDatum
  -> Maybe ScriptHash
  -> Value
  -> AMap.Map TokenName Integer
  -> Property
invariant3NFTSpent
  nft@(AssetClass (policyId, name))
  redeemer
  txInfo
  cert
  ref
  address
  datum
  referenceScript
  baseValue
  baseTokens =
    counterexample ("Context: " <> show ctx) $
      coldCommitteeScript nft ctx === True
    where
      tokens = AMap.insert name 1 baseTokens
      value = Value $ AMap.insert policyId tokens $ getValue baseValue
      txOut = TxOut address value datum referenceScript
      input = TxInInfo ref txOut
      txInfo' = txInfo{txInfoInputs = input : txInfoInputs txInfo}
      scriptInfo = CertifyingScript 0 cert
      ctx = ScriptContext txInfo' redeemer scriptInfo
