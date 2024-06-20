module CredentialManager.Scripts.HotCommitteeSpec where

import CredentialManager.Gen ()
import CredentialManager.Scripts.HotCommittee
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
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef,
  Value (Value, getValue),
  Voter,
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

invariant1BadPurpose :: AssetClass -> ScriptContext -> Property
invariant1BadPurpose nft ctx = case scriptContextScriptInfo ctx of
  VotingScript{} -> discard
  _ -> hotCommitteeScript nft ctx === False

invariant2NFTMissing :: AssetClass -> ScriptContext -> Property
invariant2NFTMissing nft ctx =
  hotCommitteeScript nft ctx' === False
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
  -> TxInfo
  -> Voter
  -> TxOutRef
  -> Redeemer
  -> Address
  -> OutputDatum
  -> Maybe ScriptHash
  -> Value
  -> AMap.Map TokenName Integer
  -> Property
invariant3NFTSpent
  nft@(AssetClass (policyId, name))
  txInfo
  voter
  ref
  redeemer
  address
  datum
  referenceScript
  baseValue
  baseTokens =
    counterexample ("Context: " <> show ctx) $
      hotCommitteeScript nft ctx === True
    where
      tokens = AMap.insert name 1 baseTokens
      value = Value $ AMap.insert policyId tokens $ getValue baseValue
      txOut = TxOut address value datum referenceScript
      input = TxInInfo ref txOut
      txInfo' = txInfo{txInfoInputs = input : txInfoInputs txInfo}
      scriptInfo = VotingScript voter
      ctx = ScriptContext txInfo' redeemer scriptInfo
