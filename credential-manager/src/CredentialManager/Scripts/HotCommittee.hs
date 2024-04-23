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

-- | The hot committee script acts as the credential authorized by the cold
-- committee credential to vote on governance actions. All it checks is that
-- the NFT provided at compile time is spent, delegating all other logic to
-- the payment credential holding the NFT.
module CredentialManager.Scripts.HotCommittee where

import PlutusLedgerApi.V3 (CurrencySymbol, TxOut (..), Value (getValue))
import PlutusTx.AssocMap (member)
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Lift as PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- | A version of PlutusLedgerApi.V3.ScriptContext that only decodes what the
-- hot committee script needs.
data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
  deriving stock (Haskell.Eq, Haskell.Show)

-- | A version of PlutusLedgerApi.V3.ScriptPurpose that only decodes what the
-- hot committee script needs.
data ScriptPurpose
  = Minting BuiltinData
  | Spending BuiltinData
  | Rewarding BuiltinData
  | Certifying BuiltinData BuiltinData
  | Voting BuiltinData
  | Proposing BuiltinData BuiltinData
  deriving stock (Haskell.Eq, Haskell.Show)

-- | A version of PlutusLedgerApi.V3.TxInfo that only decodes what the
-- hot committee script needs.
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
  deriving stock (Haskell.Show, Haskell.Eq)

-- | A version of PlutusLedgerApi.V3.TxInInfo that only decodes what the
-- hot committee script needs.
data TxInInfo = TxInInfo
  { txInInfoOutRef :: BuiltinData
  , txInInfoResolved :: TxOut
  }
  deriving stock (Haskell.Show, Haskell.Eq)

-- | This script just checks that the hard-coded currency symbol of the "Hot NFT" is
-- in any spending input of the transaction.# INLINEABLE hotCommitteeScript #-}
hotCommitteeScript :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
hotCommitteeScript symbol _ ctx = case scriptContextPurpose ctx of
  Voting _ -> any inputSpendsToken txInputs
  _ -> False
  where
    -- Checks if an input spends a token with the correct currencySymbol
    inputSpendsToken TxInInfo{txInInfoResolved} = symbol `member` getValue (txOutValue txInInfoResolved)
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
