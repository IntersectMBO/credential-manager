module CredentialManager.Scripts.HotCommitteeSpec where

import CredentialManager.Gen ()
import CredentialManager.Scripts.HotCommittee (
  ScriptContext (..),
  ScriptPurpose (..),
  TxInInfo (..),
  TxInfo (..),
  hotCommitteeScript,
 )
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  assetClassValue,
  assetClassValueOf,
 )
import PlutusLedgerApi.V3 (
  Address,
  BuiltinData,
  OutputDatum,
  ScriptHash,
  TokenName,
  TxOut (..),
  Value (Value, getValue),
 )
import qualified PlutusTx.AssocMap as AMap
import qualified PlutusTx.Prelude as P
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "Invariant 1: Fails if not voting" invariant1BadPurpose
  prop "Invariant 2: Fails if NFT not spent" invariant2NFTMissing
  prop "Invariant3: Succeeds if NFT spent" invariant3NFTSpent

invariant1BadPurpose
  :: AssetClass -> BuiltinData -> ScriptContext -> Property
invariant1BadPurpose nft redeemer ctx = case scriptContextPurpose ctx of
  Voting{} -> discard
  _ -> hotCommitteeScript nft redeemer ctx === False

invariant2NFTMissing
  :: AssetClass -> BuiltinData -> ScriptContext -> Property
invariant2NFTMissing nft redeemer ctx =
  hotCommitteeScript nft redeemer ctx' === False
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
  -> BuiltinData
  -> TxInfo
  -> BuiltinData
  -> BuiltinData
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
  voter
  ref
  address
  datum
  referenceScript
  baseValue
  baseTokens =
    counterexample ("Context: " <> show ctx) $
      hotCommitteeScript nft redeemer ctx === True
    where
      tokens = AMap.insert name 1 baseTokens
      value = Value $ AMap.insert policyId tokens $ getValue baseValue
      txOut = TxOut address value datum referenceScript
      input = TxInInfo ref txOut
      txInfo' = txInfo{txInfoInputs = input : txInfoInputs txInfo}
      purpose = Voting voter
      ctx = ScriptContext txInfo' purpose
