module TxSummary where

import Cardano.Api (
  AlonzoEraOnwards (..),
  Certificate (..),
  ConwayEra,
  ConwayEraOnwards (..),
  Featured (..),
  FileError (..),
  ShelleyBasedEra (..),
  TextEnvelopeCddlError (..),
  TxBody (..),
  TxBodyContent (..),
  TxBodyScriptData (..),
  TxCertificates (..),
  TxVotingProcedures (TxVotingProcedures, TxVotingProceduresNone),
 )
import Cardano.Api.Ledger (
  ConwayGovCert (..),
  ConwayTxCert (..),
  GovActionId (..),
  StandardCrypto,
  StrictMaybe (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import qualified Cardano.Ledger.Alonzo.Scripts as L (AsIx)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import qualified Cardano.Ledger.Conway as L (ConwayEra)
import qualified Cardano.Ledger.Conway.Scripts as L (ConwayPlutusPurpose (..))
import qualified Cardano.Ledger.Crypto as L (StandardCrypto)
import qualified Cardano.Ledger.Plutus as L
import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer (Writer, runWriter)
import CredentialManager.Api
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import PlutusLedgerApi.Common (Data (..))
import PlutusLedgerApi.V3 (
  Credential (..),
  HotCommitteeCredential (..),
  fromData,
 )
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
  -> SummaryM (TxBody ConwayEra)
checkTx result = summarize "Check transaction body file" do
  case result of
    Left err -> do
      errorStatus
      renderError err
      empty
    Right tx -> pure tx

summarizeTxBody :: TxBody ConwayEra -> SummaryM ()
summarizeTxBody txBody = do
  classification <- classifyTx txBody
  case classification of
    ColdTx redeemer -> summarizeCertificates txBody redeemer
    HotTx redeemer -> summarizeVotes txBody redeemer

summarizeVotes :: TxBody ConwayEra -> HotLockRedeemer -> SummaryM ()
summarizeVotes (TxBody TxBodyContent{..}) redeemer =
  summarize "Check transaction votes" do
    case redeemer of
      Vote ->
        case txVotingProcedures of
          Nothing -> do
            errorStatus
            describe "No votes cast"
          Just (Featured ConwayEraOnwardsConway TxVotingProceduresNone) -> do
            errorStatus
            describe "No votes cast"
          Just
            ( Featured
                ConwayEraOnwardsConway
                (TxVotingProcedures (VotingProcedures voters) _)
              ) -> case Map.toList voters of
              [] -> do
                errorStatus
                describe "No votes cast"
              [(voter, votes)]
                | Map.null votes -> do
                    errorStatus
                    describe "No votes cast"
                | otherwise -> do
                    describe $ "Voting as: " <> T.pack (show voter)
                    traverse_ (uncurry summarizeVote) (Map.toList votes)
              _ -> do
                errorStatus
                describe "Votes cast by multiple voters"
      _ ->
        case txVotingProcedures of
          Nothing -> describe "No votes cast, as expected"
          Just (Featured ConwayEraOnwardsConway TxVotingProceduresNone) -> do
            describe "No votes cast, as expected"
          Just (Featured ConwayEraOnwardsConway TxVotingProcedures{}) -> do
            errorStatus
            describe "Transaction casts votes when not allowed to"

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

summarizeVote
  :: GovActionId L.StandardCrypto
  -> VotingProcedure (L.ConwayEra StandardCrypto)
  -> ItemM ()
summarizeVote govActionId (VotingProcedure vote mAnchor) = do
  describe $
    "Vote " <> T.pack (show vote) <> " on " <> T.pack (show govActionId)
  case mAnchor of
    SNothing -> do
      warnStatus
      describe "No rationale file anchor included in vote"
    SJust anchor -> describe $ "Rationale " <> T.pack (show anchor)

summarizeCertificates :: TxBody ConwayEra -> ColdLockRedeemer -> SummaryM ()
summarizeCertificates (TxBody TxBodyContent{..}) redeemer =
  summarize "Check transaction certificates" do
    let certificatesExpected = case redeemer of
          AuthorizeHot{} -> True
          ResignCold -> True
          _ -> False
    case txCertificates of
      TxCertificatesNone
        | certificatesExpected -> do
            errorStatus
            describe "No certificates found"
        | otherwise -> do
            describe "No certificates found, as expected"
      TxCertificates _ [] _
        | certificatesExpected -> do
            errorStatus
            describe "No certificates found"
        | otherwise -> do
            describe "No certificates found, as expected"
      TxCertificates _ [certificate] _
        | certificatesExpected -> printCertificate certificate
        | otherwise -> do
            errorStatus
            describe "Unexpected certificate found:"
            printCertificate certificate
      TxCertificates{} -> do
        describe "Unexpected multiple certificates found"

printCertificate :: Certificate ConwayEra -> ItemM ()
printCertificate (ShelleyRelatedCertificate era _) = case era of {}
printCertificate (ConwayCertificate _ (ConwayTxCertDeleg delegCert)) = do
  errorStatus
  describe "Unexpected delegation certificate found"
  describe $ T.pack $ show delegCert
printCertificate (ConwayCertificate _ (ConwayTxCertPool poolCert)) = do
  errorStatus
  describe "Unexpected pool certificate found"
  describe $ T.pack $ show poolCert
printCertificate (ConwayCertificate _ (ConwayTxCertGov govCert)) =
  case govCert of
    ConwayAuthCommitteeHotKey coldCredential hotCredential -> do
      describe "Authorize committee hot credential certificate found"
      describe $ "Cold credential: " <> T.pack (show coldCredential)
      describe $ "Hot credential: " <> T.pack (show hotCredential)
    ConwayResignCommitteeColdKey coldCredential sAnchor -> do
      describe
        "Constitutional committee cold credential resignation certificate found"
      describe $ "Cold credential: " <> T.pack (show coldCredential)
      case sAnchor of
        SNothing -> do
          warnStatus
          describe "No anchor found"
        SJust anchor -> describe $ "Anchor " <> T.pack (show anchor)
    _ -> do
      errorStatus
      describe "Unexpected gov certificate found"
      describe $ T.pack $ show govCert

summarizeResignColdTx :: TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeResignColdTx = undefined

summarizeResignDelegationTx
  :: Identity -> TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeResignDelegationTx = undefined

summarizeRotateColdTx :: TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeRotateColdTx = undefined

summarizeUnlockColdTx :: TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeUnlockColdTx = undefined

summarizeVoteTx :: TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeVoteTx = undefined

summarizeResignVotingTx
  :: Identity -> TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeResignVotingTx = undefined

summarizeRotateHotTx :: TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeRotateHotTx = undefined

summarizeUnlockHotTx :: TxBody ConwayEra -> Writer [SummaryItem] ()
summarizeUnlockHotTx = undefined

classifyTx :: TxBody ConwayEra -> SummaryM TxClassification
classifyTx (ShelleyTxBody ShelleyBasedEraConway _ _ scriptData _ _) =
  summarize "Check transaction purpose" do
    case scriptData of
      TxBodyNoScriptData -> do
        errorStatus
        describe "Transaction does not execute any scripts."
        empty
      TxBodyScriptData AlonzoEraOnwardsConway _ (Redeemers redeemers) ->
        case spendingRedeemers redeemers of
          [] -> do
            errorStatus
            describe "Transaction does not spend any script outputs."
            empty
          [(L.Data rdmr, _)] -> case rdmr of
            Constr 0 [fromData -> Just (HotCommitteeCredential hotCred)] -> do
              describe "Hot credential authorization transaction."
              case hotCred of
                ScriptCredential hash ->
                  describe $ "Hot credential script hash: " <> T.pack (show hash) <> "."
                PubKeyCredential hash -> do
                  warnStatus
                  describe $ "Hot credential pub key hash: " <> T.pack (show hash) <> "."
                  describe "This is unusual, normally a script credential should be authorized."
              pure $ ColdTx $ AuthorizeHot $ HotCommitteeCredential hotCred
            Constr 1 [] -> do
              describe "Constitutional committee resignation transaction."
              pure $ ColdTx ResignCold
            Constr 2 [fromData -> Just Identity{..}] -> do
              describe "Delegate resignation transaction."
              describe $ "Resignee certificate hash: " <> T.pack (show certificateHash)
              describe $ "Resignee public key hash: " <> T.pack (show pubKeyHash)
              pure $ ColdTx $ ResignDelegation Identity{..}
            Constr 3 [] -> do
              describe "Cold credential key rotation transaction."
              pure $ ColdTx RotateCold
            Constr 4 [] -> do
              describe "Cold NFT unlock transaction."
              pure $ ColdTx UnlockCold
            Constr 5 [] -> do
              describe "Vote transaction."
              pure $ HotTx Vote
            Constr 6 [fromData -> Just Identity{..}] -> do
              describe "Voter resignation transaction."
              describe $ "Resignee certificate hash: " <> T.pack (show certificateHash)
              describe $ "Resignee public key hash: " <> T.pack (show pubKeyHash)
              pure $ HotTx $ ResignVoting Identity{..}
            Constr 7 [] -> do
              describe "Hot credential key rotation transaction."
              pure $ HotTx RotateHot
            Constr 8 [] -> do
              describe "Hot NFT unlock transaction."
              pure $ HotTx UnlockHot
            _ -> do
              errorStatus
              describe "Transaction has invalid redeemer datum."
              empty
          _ -> do
            errorStatus
            describe "Transaction spends multiple script outputs."
            empty

classificationStatus :: TxClassification -> ItemStatus
classificationStatus = \case
  ColdTx (AuthorizeHot (HotCommitteeCredential ScriptCredential{})) -> Ok
  ColdTx (AuthorizeHot (HotCommitteeCredential PubKeyCredential{})) -> Warn
  ColdTx ResignCold -> Ok
  ColdTx ResignDelegation{} -> Ok
  ColdTx RotateCold -> Ok
  ColdTx UnlockCold -> Warn
  HotTx Vote -> Ok
  HotTx ResignVoting{} -> Ok
  HotTx RotateHot -> Ok
  HotTx UnlockHot -> Warn

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
