module Main where

import Cardano.Api (
  AlonzoEraOnwards (..),
  AsType (..),
  AssetId (..),
  Certificate (..),
  ConwayEra,
  ConwayEraOnwards (..),
  CtxTx,
  Featured (..),
  File (..),
  FileError (..),
  Hash,
  HashableScriptData,
  Key (verificationKeyHash),
  PaymentKey,
  Quantity (..),
  SerialiseAddress (serialiseAddress),
  SerialiseAsRawBytes (deserialiseFromRawBytes),
  ShelleyBasedEra (..),
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  TextEnvelopeError (..),
  TextEnvelopeType (..),
  TxBody (..),
  TxBodyContent (..),
  TxBodyScriptData (..),
  TxCertificates (..),
  TxExtraKeyWitnesses (..),
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  TxProposalProcedures (..),
  TxVotingProcedures (..),
  getScriptData,
  getTxBody,
  makeShelleyKeyWitness,
  readFileTextEnvelope,
  selectLovelace,
  serialiseToRawBytesHexText,
  txOutValueToValue,
  valueToList,
  writeTxWitnessFileTextEnvelopeCddl,
 )
import Cardano.Api.Ledger (
  ConwayGovCert (..),
  ConwayTxCert (..),
  GovActionId,
  StandardCrypto,
  StrictMaybe (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import qualified Cardano.Api.Ledger as L
import Cardano.Api.Shelley (TxWithdrawals (..), toPlutusData)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import qualified Cardano.Ledger.Conway as L
import qualified Cardano.Ledger.Conway.Core as L
import qualified Cardano.Ledger.Conway.Scripts as L
import qualified Cardano.Ledger.Plutus as L
import Control.Exception (Exception (..))
import Control.Monad (unless, when)
import CredentialManager.Api (
  ColdLockDatum (..),
  ColdLockRedeemer (..),
  HotLockDatum (..),
  HotLockRedeemer (..),
  Identity (..),
  parsePrivateKeyFromPEMBytes,
 )
import Crypto.PubKey.Ed25519 (toPublic)
import qualified Data.ByteArray as BA
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Foldable (Foldable (..), for_, traverse_)
import qualified Data.Map as Map
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Traversable (for)
import Data.Version (showVersion)
import Formatting.Buildable (Buildable (..))
import Options.Applicative
import Paths_credential_manager (version)
import PlutusLedgerApi.Common (Data (..), fromData)
import PlutusLedgerApi.V3 (Credential (..), HotCommitteeCredential (..))
import System.Directory (doesFileExist)
import System.Exit (die, exitFailure)
import System.IO (hFlush, stdout)
import System.Process (readProcess)

main :: IO ()
main = do
  Options{..} <- execParser options
  let checkExists file = do
        exists <- doesFileExist file
        unless exists $ die $ "File " <> file <> " does not exist"
  checkExists signingKeyFile
  checkExists txBodyFile
  decryptedPEMContent <- readProcess "openssl" ["pkey", "-in", signingKeyFile] ""
  key <-
    either die pure $ parsePrivateKeyFromPEMBytes $ fromString decryptedPEMContent
  let signingKey =
        fromRight
          ( error
              "Failed to convert from crypton secret key to cardano-api signing key"
          )
          . deserialiseFromRawBytes (AsSigningKey AsPaymentKey)
          . BA.convert
          $ key
  let pubKeyHash =
        verificationKeyHash
          . fromRight
            ( error
                "Failed to convert from crypton public key to cardano-api verification key"
            )
          . deserialiseFromRawBytes (AsVerificationKey AsPaymentKey)
          . BA.convert
          . toPublic
          $ key
  putStr "Checking transaction body file... "
  txResult <- readFileTextEnvelope (AsTx AsConwayEra) (File txBodyFile)
  tx <- case txResult of
    Left err -> do
      renderError err
      exitFailure
    Right tx -> do
      putStrLn "OK"
      pure tx
  let txBody = getTxBody tx
  classification <- classifyTx txBody
  promptToProceed "Is the transaction doing what you expect?"
  promptCertificates <- summarizeCertificates txBody classification
  when promptCertificates $ promptToProceed "Is this certificate correct?"
  promptVotes <- summarizeVotes txBody classification
  when promptVotes $ promptToProceed "Are these votes correct?"
  summarizeOutputs classification txBody
  checkExtraTxBodyFields txBody
  summarizeSignatories pubKeyHash txBody
  promptToProceed "Do you wish to sign this transaction?"
  let witness =
        makeShelleyKeyWitness ShelleyBasedEraConway txBody $
          WitnessPaymentKey signingKey
  saveResult <-
    writeTxWitnessFileTextEnvelopeCddl ShelleyBasedEraConway (File outFile) witness
  case saveResult of
    Left err -> die $ show err
    Right _ -> putStrLn $ "Saved witness to " <> outFile

promptToProceed :: String -> IO ()
promptToProceed msg = do
  putStr $ msg <> " (yN): "
  hFlush stdout
  response <- getLine
  unless (fmap toLower response == "y") exitFailure

data Options = Options
  { signingKeyFile :: FilePath
  , txBodyFile :: FilePath
  , outFile :: FilePath
  }
  deriving (Show)

options :: ParserInfo Options
options = info parser description

parser :: Parser Options
parser = helper <*> versionInfo <*> optionsParser

optionsParser :: Parser Options
optionsParser =
  Options
    <$> signingKeyFileParser
    <*> txBodyFileParser
    <*> outFileParser

versionInfo :: Parser (Options -> Options)
versionInfo =
  infoOption
    ("cc-sign " <> showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

description :: InfoMod Options
description =
  fold
    [ fullDesc
    , progDesc "CLI application for signing CC Credential Manager transactions."
    ]

signingKeyFileParser :: Parser FilePath
signingKeyFileParser =
  strOption $
    fold
      [ long "private-key-file"
      , short 'k'
      , metavar "FILEPATH"
      , help "The PEM file containing your encrypted private key"
      , action "file"
      ]

txBodyFileParser :: Parser FilePath
txBodyFileParser =
  strOption $
    fold
      [ long "tx-body-file"
      , short 't'
      , metavar "FILEPATH"
      , help "The file containing the transaction body to sign."
      , action "file"
      ]

outFileParser :: Parser FilePath
outFileParser =
  strOption $
    fold
      [ long "out-file"
      , short 'o'
      , metavar "FILEPATH"
      , help "A file path to save the transaction witness file."
      , action "file"
      ]

printIndented :: String -> IO ()
printIndented s = putStrLn $ "  " <> s

printTitle :: String -> IO ()
printTitle s = do
  putStrLn ""
  putStrLn s

dieIndented :: String -> IO a
dieIndented s = die $ "  " <> s

classifyTx :: TxBody ConwayEra -> IO TxClassification
classifyTx (ShelleyTxBody ShelleyBasedEraConway _ _ scriptData _ _) = do
  printTitle "Checking transaction purpose..."
  case scriptData of
    TxBodyNoScriptData -> dieIndented "Transaction does not execute any scripts."
    TxBodyScriptData AlonzoEraOnwardsConway _ (Redeemers redeemers) ->
      case spendingRedeemers redeemers of
        [] -> dieIndented "Transaction does not spend any script outputs."
        [(L.Data rdmr, _)] -> case rdmr of
          Constr 0 [fromData -> Just (HotCommitteeCredential hotCred)] -> do
            printIndented "Hot credential authorization transaction."
            case hotCred of
              ScriptCredential hash ->
                printIndented $ "Hot credential script hash: " <> show hash <> "."
              PubKeyCredential hash -> do
                printIndented $ "Hot credential pub key hash: " <> show hash <> "."
                printIndented
                  "WARNING: This is unusual, normally a script credential should be authorized."
            pure $ ColdTx $ AuthorizeHot $ HotCommitteeCredential hotCred
          Constr 1 [] -> do
            printIndented "Constitutional committee resignation transaction."
            pure $ ColdTx ResignCold
          Constr 2 [fromData -> Just Identity{..}] -> do
            printIndented "Membership resignation transaction."
            printIndented $ "Resignee certificate hash: " <> show certificateHash
            printIndented $ "Resignee public key hash: " <> show pubKeyHash
            pure $ ColdTx $ ResignDelegation Identity{..}
          Constr 3 [fromData -> Just Identity{..}] -> do
            printIndented "Delegate resignation transaction."
            printIndented $ "Resignee certificate hash: " <> show certificateHash
            printIndented $ "Resignee public key hash: " <> show pubKeyHash
            pure $ ColdTx $ ResignDelegation Identity{..}
          Constr 4 [] -> do
            printIndented "Cold credential key rotation transaction."
            pure $ ColdTx RotateCold
          Constr 5 [] -> do
            printIndented "Cold NFT burn transaction."
            pure $ ColdTx BurnCold
          Constr 6 [fromData -> Just scriptHash] -> do
            printIndented "Cold NFT upgrade transaction."
            printIndented $ "New lock script hash: " <> show scriptHash
            pure $ ColdTx $ UpgradeCold scriptHash
          Constr 7 [] -> do
            printIndented "Vote transaction."
            pure $ HotTx Vote
          Constr 8 [fromData -> Just Identity{..}] -> do
            printIndented "Voter resignation transaction."
            printIndented $ "Resignee certificate hash: " <> show certificateHash
            printIndented $ "Resignee public key hash: " <> show pubKeyHash
            pure $ HotTx $ ResignVoting Identity{..}
          Constr 9 [] -> do
            printIndented "Hot credential key rotation transaction."
            pure $ HotTx RotateHot
          Constr 10 [] -> do
            printIndented "Hot NFT burn transaction."
            pure $ ColdTx BurnCold
          Constr 11 [fromData -> Just scriptHash] -> do
            printIndented "Hot NFT upgrade transaction."
            printIndented $ "New lock script hash: " <> show scriptHash
            pure $ ColdTx $ UpgradeCold scriptHash
          _ -> do
            die "Transaction has invalid redeemer datum."
        _ -> do
          die "Transaction spends multiple script outputs."

data TxClassification
  = ColdTx ColdLockRedeemer
  | HotTx HotLockRedeemer
  deriving (Show)

summarizeCertificates :: TxBody ConwayEra -> TxClassification -> IO Bool
summarizeCertificates (TxBody TxBodyContent{..}) classification = do
  printTitle "Check transaction certificates..."
  let certificatesExpected = case classification of
        ColdTx AuthorizeHot{} -> True
        ColdTx ResignCold -> True
        _ -> False
  case txCertificates of
    TxCertificatesNone
      | certificatesExpected -> do
          dieIndented "No certificates found"
      | otherwise -> do
          printIndented "No certificates found, as expected"
          pure False
    TxCertificates _ [] _
      | certificatesExpected -> do
          dieIndented "No certificates found"
      | otherwise -> do
          printIndented "No certificates found, as expected"
          pure False
    TxCertificates _ [certificate] _
      | certificatesExpected -> do
          printCertificate certificate
          pure True
      | otherwise -> do
          printIndented "Unexpected certificate found:"
          printCertificate certificate
          exitFailure
    TxCertificates{} -> do
      dieIndented "Unexpected multiple certificates found"

printCertificate :: Certificate ConwayEra -> IO ()
printCertificate (ShelleyRelatedCertificate era _) = case era of {}
printCertificate (ConwayCertificate _ (ConwayTxCertDeleg delegCert)) = do
  printIndented "Unexpected delegation certificate found"
  dieIndented $ show delegCert
printCertificate (ConwayCertificate _ (ConwayTxCertPool poolCert)) = do
  printIndented "Unexpected pool certificate found"
  dieIndented $ show poolCert
printCertificate (ConwayCertificate _ (ConwayTxCertGov govCert)) =
  case govCert of
    ConwayAuthCommitteeHotKey coldCredential hotCredential -> do
      printIndented "Authorize committee hot credential certificate found"
      printIndented $ "Cold credential: " <> show coldCredential
      printIndented $ "Hot credential: " <> show hotCredential
    ConwayResignCommitteeColdKey coldCredential sAnchor -> do
      printIndented
        "Constitutional committee cold credential resignation certificate found"
      printIndented $ "Cold credential: " <> show coldCredential
      case sAnchor of
        SNothing -> do
          printIndented "WARNING: No anchor found"
        SJust anchor -> printIndented $ "Anchor " <> show anchor
    _ -> do
      printIndented "Unexpected gov certificate found"
      dieIndented $ show govCert

summarizeVotes :: TxBody ConwayEra -> TxClassification -> IO Bool
summarizeVotes (TxBody TxBodyContent{..}) classification = do
  printTitle "Check transaction votes..."
  case classification of
    HotTx Vote ->
      case txVotingProcedures of
        Nothing -> do
          dieIndented "No votes cast"
        Just (Featured ConwayEraOnwardsConway TxVotingProceduresNone) -> do
          dieIndented "No votes cast"
        Just
          ( Featured
              ConwayEraOnwardsConway
              (TxVotingProcedures (VotingProcedures voters) _)
            ) -> case Map.toList voters of
            [] -> do
              dieIndented "No votes cast"
            [(voter, votes)]
              | Map.null votes -> do
                  dieIndented "No votes cast"
              | otherwise -> do
                  printIndented $ "Voting as: " <> show voter
                  traverse_ (uncurry summarizeVote) (Map.toList votes)
                  pure True
            _ -> do
              dieIndented "Votes cast by multiple voters"
    _ ->
      case txVotingProcedures of
        Nothing -> do
          printIndented "No votes cast, as expected"
          pure False
        Just (Featured ConwayEraOnwardsConway TxVotingProceduresNone) -> do
          printIndented "No votes cast, as expected"
          pure False
        Just
          ( Featured
              ConwayEraOnwardsConway
              (TxVotingProcedures (VotingProcedures voters) _)
            )
            | Map.null voters -> do
                printIndented "No votes cast, as expected"
                pure False
            | otherwise -> do
                dieIndented "Transaction casts votes when not allowed to"

summarizeVote
  :: GovActionId L.StandardCrypto
  -> VotingProcedure (L.ConwayEra StandardCrypto)
  -> IO ()
summarizeVote govActionId (VotingProcedure vote mAnchor) = do
  printIndented $ "Vote " <> show vote <> " on " <> show govActionId
  case mAnchor of
    SNothing -> printIndented "WARNING: No rationale file anchor included in vote"
    SJust anchor -> printIndented $ "Rationale " <> show anchor

summarizeOutputs :: TxClassification -> TxBody ConwayEra -> IO ()
summarizeOutputs classification (TxBody TxBodyContent{..}) =
  for_ (zip @Int [0 ..] txOuts) \(ix, txOut) -> do
    printTitle $ "Check transaction output #" <> show ix <> "..."
    summarizeOutput classification txOut
    promptToProceed "Is this output OK?"

summarizeOutput :: TxClassification -> TxOut CtxTx ConwayEra -> IO ()
summarizeOutput classification (TxOut address outValue outDatum _refScript) = do
  let val = txOutValueToValue outValue
  printIndented $ "Send to address " <> T.unpack (serialiseAddress address)
  printIndented $ show (L.unCoin $ selectLovelace val) <> " Lovelace"
  for_ (valueToList val) \case
    (AdaAssetId, _) -> pure ()
    (AssetId p n, Quantity q) ->
      printIndented $
        show q
          <> " "
          <> T.unpack (serialiseToRawBytesHexText p)
          <> case T.unpack (serialiseToRawBytesHexText n) of
            "" -> ""
            n' -> "." <> n'
  case outDatum of
    TxOutDatumNone ->
      pure ()
    TxOutDatumHash _ scriptHash -> do
      dieIndented $
        "Unexpected output datum hash: "
          <> T.unpack (serialiseToRawBytesHexText scriptHash)
    TxOutDatumInTx _ datum ->
      summarizeDatum classification datum
    TxOutDatumInline _ datum ->
      summarizeDatum classification datum

summarizeDatum :: TxClassification -> HashableScriptData -> IO ()
summarizeDatum classification datum = do
  let plutusDatum = toPlutusData $ getScriptData datum
  case fromData plutusDatum of
    Just ColdLockDatum{..} -> do
      printIndented "Cold NFT datum found"
      case classification of
        ColdTx RotateCold -> do
          printIndented "New membership keys:"
          for_ membershipUsers summarizeIdentity
          printIndented "New delegation keys:"
          for_ delegationUsers summarizeIdentity
        ColdTx _ -> pure ()
        HotTx _ -> do
          dieIndented "Not expected in a hot credential transaction"
    Nothing -> case fromData plutusDatum of
      Just HotLockDatum{..} -> do
        printIndented "Hot NFT datum found"
        case classification of
          HotTx RotateHot -> do
            printIndented "New voting keys:"
            for_ votingUsers summarizeIdentity
          HotTx _ -> pure ()
          ColdTx _ -> do
            dieIndented "Not expected in a cold credential transaction"
      Nothing -> do
        printIndented "Unrecognized datum in output"
        dieIndented $ show datum

summarizeIdentity :: Identity -> IO ()
summarizeIdentity Identity{..} = do
  printIndented $ "⋅ public key hash: " <> show pubKeyHash
  printIndented $ "  certificate hash: " <> show certificateHash

checkExtraTxBodyFields :: TxBody ConwayEra -> IO ()
checkExtraTxBodyFields (TxBody TxBodyContent{..}) = do
  printTitle "Check extra tx body fields..."
  case txWithdrawals of
    TxWithdrawalsNone -> pure ()
    TxWithdrawals _ [] -> pure ()
    TxWithdrawals _ _ -> do
      printIndented "WARNING: Transaction unexpectedly withdraws staking rewards."
      promptToProceed "Are you OK with this?"
  case txMintValue of
    TxMintNone -> pure ()
    TxMintValue _ val _
      | val == mempty -> pure ()
      | otherwise -> do
          printIndented "WARNING: Transaction unexpectedly mints or burns tokens."
          promptToProceed "Are you OK with this?"
  case txProposalProcedures of
    Nothing -> pure ()
    Just (Featured _ TxProposalProceduresNone) -> pure ()
    Just (Featured _ (TxProposalProcedures proposalProcedures _))
      | proposalProcedures == mempty -> pure ()
      | otherwise -> do
          printIndented "WARNING Transaction unexpectedly submits governance actions."
          promptToProceed "Are you OK with this?"

summarizeSignatories :: Hash PaymentKey -> TxBody ConwayEra -> IO ()
summarizeSignatories myKey (TxBody TxBodyContent{..}) = do
  printTitle "Check transaction signatories..."
  case txExtraKeyWits of
    TxExtraKeyWitnessesNone -> dieIndented "This transaction requires no signatures"
    TxExtraKeyWitnesses _ signatories -> do
      canSign <-
        or <$> for signatories \pkh -> do
          let canSign = pkh == myKey
          printIndented $
            "Requires signature from "
              <> T.unpack (serialiseToRawBytesHexText pkh)
              <> if canSign
                then " (you can sign)"
                else ""
          pure canSign
      unless canSign do
        dieIndented "You cannot sign this transaction."

spendingRedeemers
  :: Map.Map (L.ConwayPlutusPurpose L.AsIx (L.ConwayEra L.StandardCrypto)) a -> [a]
spendingRedeemers = Map.elems . Map.filterWithKey (const . isSpendingPurpose)

isSpendingPurpose
  :: L.ConwayPlutusPurpose L.AsIx (L.ConwayEra L.StandardCrypto) -> Bool
isSpendingPurpose = \case
  L.ConwaySpending{} -> True
  _ -> False

renderError :: FileError TextEnvelopeError -> IO ()
renderError = \case
  FileError _ (TextEnvelopeTypeError expected (TextEnvelopeType received)) -> do
    putStrLn "Text envelope \"type\" field invalid. Expected one of:"
    traverse_ (putStrLn . ("⋅ " <>) . fromString . unTextEnvelopeType) expected
    putStrLn $ "But got " <> fromString received
  FileError _ (TextEnvelopeDecodeError err) -> do
    putStrLn "Text envelope file contains invalid tx body binary data. Details:"
    traverse_ putStrLn $ lines (TL.unpack $ TLB.toLazyText $ build err)
  FileError _ (TextEnvelopeAesonDecodeError err) -> do
    putStrLn "Text envelope file contains invalid JSON. Details:"
    putStrLn err
  FileErrorTempFile{} -> putStrLn "Unable to open temporary file"
  FileDoesNotExistError{} -> putStrLn "File does not exist"
  FileIOError _ ioErr -> do
    putStrLn "Error reading file:"
    putStrLn $ displayException ioErr

unTextEnvelopeType :: TextEnvelopeType -> String
unTextEnvelopeType (TextEnvelopeType t) = t
