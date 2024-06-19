{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

-- | The cold committee script acts as the credential that is listed in the
-- committee state on-chain and used to authorize hot credentials. All it
-- checks is that the NFT provided at compile time is spent, delegating all
-- other logic to the payment credential holding the NFT.
module CredentialManager.Scripts.ColdCommittee where

import CredentialManager.Scripts.Common (trace, traceIfFalse)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValueOf)
import PlutusLedgerApi.V3 (TxOut (..))
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Lift as PlutusTx
import PlutusTx.Prelude hiding (trace, traceIfFalse)
import qualified Prelude as Haskell

-- | A version of PlutusLedgerApi.V3.ScriptContext that only decodes what the
-- cold committee script needs.
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

-- | A version of PlutusLedgerApi.V3.ScriptPurpose that only decodes what the
-- cold committee script needs.
data ScriptPurpose
  = Minting BuiltinData
  | Spending BuiltinData
  | Rewarding BuiltinData
  | Certifying BuiltinData BuiltinData
  | Voting BuiltinData
  | Proposing BuiltinData BuiltinData
  deriving stock (Haskell.Eq, Haskell.Show, Generic)

-- | A version of PlutusLedgerApi.V3.TxInfo that only decodes what the
-- cold committee script needs.
data TxInfo = TxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: BuiltinData
  , txInfoOutputs :: BuiltinData
  , txInfoFee :: BuiltinData
  , txInfoMint :: BuiltinData
  , txInfoTxCerts :: BuiltinData
  , txInfoWdrl :: BuiltinData
  , txInfoValidRange :: BuiltinData
  , txInfoSignatories :: BuiltinData
  , txInfoRedeemers :: BuiltinData
  , txInfoData :: BuiltinData
  , txInfoId :: BuiltinData
  , txInfoVotes :: BuiltinData
  , txInfoProposalProcedures :: BuiltinData
  , txInfoCurrentTreasuryAmount :: BuiltinData
  , txInfoTreasuryDonation :: BuiltinData
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

-- | A version of PlutusLedgerApi.V3.TxInInfo that only decodes what the
-- cold committee script needs.
data TxInInfo = TxInInfo
  { txInInfoOutRef :: BuiltinData
  , txInInfoResolved :: TxOut
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

-- | This script just checks that the hard-coded currency symbol of the NFT is
-- in any spending input of the transaction.
{-# INLINEABLE coldCommitteeScript #-}
coldCommitteeScript :: AssetClass -> BuiltinData -> ScriptContext -> Bool
coldCommitteeScript nft _ ctx = case scriptContextPurpose ctx of
  Certifying _ _ ->
    traceIfFalse "Cold NFT not found in any input" $ any inputSpendsToken txInputs
  _ -> trace "Invalid script purpose" False
  where
    -- Checks if an input spends the correct token
    inputSpendsToken TxInInfo{txInInfoResolved = TxOut{..}} =
      assetClassValueOf txOutValue nft == 1
    -- The list of transaction inputs being consumed in this transaction.
    txInputs = txInfoInputs (scriptContextTxInfo ctx)

PlutusTx.makeLift ''ScriptPurpose
PlutusTx.makeIsDataIndexed
  ''ScriptPurpose
  [ ('Minting, 0)
  , ('Spending, 1)
  , ('Rewarding, 2)
  , ('Certifying, 3)
  , ('Voting, 4)
  , ('Proposing, 5)
  ]

PlutusTx.makeLift ''TxInInfo
PlutusTx.makeIsDataIndexed ''TxInInfo [('TxInInfo, 0)]

PlutusTx.makeLift ''TxInfo
PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

PlutusTx.makeLift ''ScriptContext
PlutusTx.makeIsDataIndexed ''ScriptContext [('ScriptContext, 0)]
