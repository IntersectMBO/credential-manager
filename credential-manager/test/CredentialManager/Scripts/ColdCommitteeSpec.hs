module CredentialManager.Scripts.ColdCommitteeSpec where

import CredentialManager.Gen ()
import CredentialManager.Scripts.ColdCommittee (
  ScriptContext (..),
  ScriptPurpose (..),
  TxInInfo (..),
  TxInfo (..),
  coldCommitteeScript,
 )
import PlutusLedgerApi.V3 (
  Address,
  BuiltinData,
  CurrencySymbol,
  OutputDatum,
  ScriptHash,
  TokenName,
  TxOut (..),
  Value (Value, getValue),
 )
import qualified PlutusTx.AssocMap as AMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "Invariant 1: Fails if not certifying" invariant1BadPurpose
  prop "Invariant 2: Fails if NFT not spent" invariant2NFTMissing
  prop "Invariant 2: Succeeds if NFT spent" invariant3NFTSpent

invariant1BadPurpose
  :: CurrencySymbol -> BuiltinData -> ScriptContext -> Property
invariant1BadPurpose policyId redeemer ctx = case scriptContextPurpose ctx of
  Certifying{} -> discard
  _ -> coldCommitteeScript policyId redeemer ctx === False

invariant2NFTMissing
  :: CurrencySymbol -> BuiltinData -> ScriptContext -> Property
invariant2NFTMissing policyId redeemer ctx =
  coldCommitteeScript policyId redeemer ctx' === False
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
                              { txOutValue = Value $ AMap.delete policyId $ getValue txOutValue
                              , ..
                              }
                        , ..
                        }
                }
        }

invariant3NFTSpent
  :: CurrencySymbol
  -> BuiltinData
  -> TxInfo
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> Address
  -> OutputDatum
  -> Maybe ScriptHash
  -> Value
  -> AMap.Map TokenName Integer
  -> TokenName
  -> Integer
  -> Property
invariant3NFTSpent
  policyId
  redeemer
  txInfo
  index
  cert
  ref
  address
  datum
  referenceScript
  baseValue
  baseTokens
  token
  quantity =
    counterexample ("Context: " <> show ctx) $
      coldCommitteeScript policyId redeemer ctx === True
    where
      tokens = AMap.insert token (max 1 quantity) baseTokens
      value = Value $ AMap.insert policyId tokens $ getValue baseValue
      txOut = TxOut address value datum referenceScript
      input = TxInInfo ref txOut
      txInfo' = txInfo{txInfoInputs = input : txInfoInputs txInfo}
      purpose = Certifying index cert
      ctx = ScriptContext txInfo' purpose
