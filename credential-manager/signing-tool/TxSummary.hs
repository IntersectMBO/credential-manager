module TxSummary where

import Cardano.Api (
  AlonzoEraOnwards (..),
  ConwayEra,
  FileError (..),
  ShelleyBasedEra (..),
  TextEnvelopeCddlError (..),
  TxBody (..),
  TxBodyScriptData (..),
 )
import qualified Cardano.Ledger.Alonzo.Scripts as L (AsIx)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import qualified Cardano.Ledger.Conway as L (ConwayEra)
import qualified Cardano.Ledger.Conway.Scripts as L (ConwayPlutusPurpose (..))
import qualified Cardano.Ledger.Crypto as L (StandardCrypto)
import qualified Cardano.Ledger.Plutus as L
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import CredentialManager.Api
import Data.Map (Map)
import qualified Data.Map as Map
import PlutusLedgerApi.Common (Data (..))
import PlutusLedgerApi.V3 (fromData)
import TxSummary.Common
import TxSummary.Error (renderError)

summarizeTx
  :: Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra)
  -> [SummaryItem]
summarizeTx result = evalWriter $ runMaybeT do
  tx <- checkTx result
  summarizeTxBody tx
  pure ()

evalWriter :: Writer w a -> w
evalWriter = snd . runWriter

checkTx
  :: Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra)
  -> MaybeT (Writer [SummaryItem]) (TxBody ConwayEra)
checkTx result = MaybeT do
  let (details, mTx) = case result of
        Left err -> (renderError err, Nothing)
        Right tx -> ([], Just tx)
  tell $
    pure $
      SummaryItem
        (maybe Error (const Ok) mTx)
        "Check transaction body file"
        details
  pure mTx

summarizeTxBody :: TxBody ConwayEra -> MaybeT (Writer [SummaryItem]) ()
summarizeTxBody txBody = do
  _classification <- classifyTx txBody
  pure ()

classifyTx :: TxBody ConwayEra -> MaybeT (Writer [SummaryItem]) TxClassification
classifyTx (ShelleyTxBody ShelleyBasedEraConway _ _ scriptData _ _) = MaybeT do
  (classification, details) <- case scriptData of
    TxBodyNoScriptData -> pure (Nothing, ["Transaction does not execute any scripts."])
    TxBodyScriptData AlonzoEraOnwardsConway _ (Redeemers redeemers) ->
      case spendingRedeemers redeemers of
        [] -> pure (Nothing, ["Transaction does not spend any script outputs."])
        [(L.Data rdmr, _)] -> case rdmr of
          Constr 0 [fromData -> Just hotCred] ->
            pure
              ( Just $ ColdTx $ AuthorizeHot hotCred
              , ["Hot credential authorization transaction."]
              )
          Constr 1 [] ->
            pure
              ( Just $ ColdTx ResignCold
              , ["Constitutional committee resignation transaction."]
              )
          Constr 2 [fromData -> Just delegate] ->
            pure
              ( Just $ ColdTx $ ResignDelegation delegate
              , ["Delegate resignation transaction."]
              )
          Constr 3 [] ->
            pure
              ( Just $ ColdTx RotateCold
              , ["Cold credential key rotation transaction."]
              )
          Constr 4 [] ->
            pure
              ( Just $ ColdTx UnlockCold
              , ["Cold NFT unlock transaction."]
              )
          Constr 5 [] -> pure (Just $ HotTx Vote, ["Vote transaction."])
          Constr 6 [fromData -> Just voter] ->
            pure
              ( Just $ HotTx $ ResignVoting voter
              , ["Voter resignation transaction."]
              )
          Constr 7 [] ->
            pure
              ( Just $ HotTx RotateHot
              , ["Hot credential key rotation transaction."]
              )
          Constr 8 [] ->
            pure
              ( Just $ HotTx UnlockHot
              , ["Hot NFT unlock transaction."]
              )
          _ -> pure (Nothing, ["Transaction has invalid redeemer datum."])
        _ -> pure (Nothing, ["Transaction spends multiple script outputs."])
  tell $
    pure $
      SummaryItem
        (maybe Error (const Ok) classification)
        "Check transaction purpose"
        details
  pure classification

spendingRedeemers
  :: Map (L.ConwayPlutusPurpose L.AsIx (L.ConwayEra L.StandardCrypto)) a -> [a]
spendingRedeemers = Map.elems . Map.filterWithKey (const . isSpendingPurpose)

isSpendingPurpose
  :: L.ConwayPlutusPurpose L.AsIx (L.ConwayEra L.StandardCrypto) -> Bool
isSpendingPurpose = \case
  L.ConwaySpending{} -> True
  _ -> False

data TxClassification
  = ColdTx ColdLockRedeemer
  | HotTx HotLockRedeemer
