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
import Cardano.Ledger.Conway.Governance (govActionIdToText)
import qualified Cardano.Ledger.Conway.Scripts as L
import qualified Cardano.Ledger.Plutus as L
import Cardano.TxDynamic (
  GroupMemHeader (..),
  SomeTxBundle (..),
  TxBundle (..),
  signBundle,
 )
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
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (decodeFileOrFail, encodeFile)
import Data.BitSet (BitSet)
import qualified Data.BitSet as BS
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
import qualified Data.Vector.Unboxed as U
import Data.Version (showVersion)
import Formatting.Buildable (Buildable (..))
import Options.Applicative
import Paths_credential_manager (version)
import PlutusLedgerApi.Common (Data (..), fromData)
import PlutusLedgerApi.V3 (Credential (..), HotCommitteeCredential (..))
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Exit (die, exitFailure)
import System.IO (hFlush, hSetEcho, stdin, stdout)
import System.Process (readProcess)
import Witherable (Witherable (witherMap))

main :: IO ()
main = do
  Options{..} <- execParser options
  let checkExists file = do
        exists <- doesFileExist file
        unless exists $ die $ "File " <> file <> " does not exist"
  checkExists signingKeyFile
  either checkExists checkExists txBodyOrBundleFile
  putStr $ "Enter pass phrase for " <> signingKeyFile <> ": "
  hFlush stdout
  hSetEcho stdin False
  password <- getLine
  putStrLn ""
  hSetEcho stdin True
  decryptedPEMContent <-
    readProcess
      "openssl"
      ["pkey", "-in", signingKeyFile, "-passin", "stdin"]
      password
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
  unless (BS.member Quiet flags) $ putStr "Checking transaction body file... "
  txOrBundle <- case txBodyOrBundleFile of
    Left txBodyFile ->
      Left <$> do
        txResult <- readFileTextEnvelope (AsTx AsConwayEra) (File txBodyFile)
        case txResult of
          Left err -> do
            renderError err
            exitFailure
          Right tx -> do
            unless (BS.member Quiet flags) $ putStrLn "OK"
            pure tx
    Right txBundleFile ->
      Right <$> do
        result <- decodeFileOrFail txBundleFile
        case result of
          Left err -> die $ show err
          Right (SomeTxBundle ShelleyBasedEraConway bundle) -> pure bundle
          Right _ -> die "Tx bundle file is not in Conway era"
  let txBody = either getTxBody tbTxBody txOrBundle
  classification <- classifyTx flags txBody
  promptToProceed flags "Is the transaction doing what you expect?"
  promptCertificates <- summarizeCertificates flags txBody classification
  when promptCertificates $ promptToProceed flags "Is this certificate correct?"
  promptVotes <- summarizeVotes flags txBody classification
  when promptVotes $ promptToProceed flags "Are these votes correct?"
  summarizeOutputs flags pubKeyHash classification txBody
  checkExtraTxBodyFields classification flags txBody
  summarizeSignatories flags pubKeyHash $ first (const txBody) txOrBundle
  promptToProceed flags "Do you wish to sign this transaction?"
  case txOrBundle of
    Left _ -> do
      let witness =
            makeShelleyKeyWitness ShelleyBasedEraConway txBody $
              WitnessPaymentKey signingKey
      saveResult <-
        writeTxWitnessFileTextEnvelopeCddl ShelleyBasedEraConway (File outFile) witness
      case saveResult of
        Left err -> die $ show err
        Right _ -> unless (BS.member Quiet flags) $ putStrLn $ "Saved witness to " <> outFile
    Right bundle -> do
      witBundle <- case signBundle signingKey bundle of
        Left err -> die $ show err
        Right x -> pure x
      encodeFile outFile witBundle
      unless (BS.member Quiet flags) $
        putStrLn $
          "Saved witness bundle to " <> outFile

promptToProceed :: BitSet Flags -> String -> IO ()
promptToProceed flags msg
  | BS.member Yes flags || BS.member Quiet flags = pure ()
  | otherwise = do
      putStr $ msg <> " (yN): "
      hFlush stdout
      response <- getLine
      unless (fmap toLower response == "y") exitFailure

data Flags
  = Quiet
  | Yes
  deriving (Show, Eq, Ord, Enum)

data Options = Options
  { flags :: BitSet Flags
  , signingKeyFile :: FilePath
  , txBodyOrBundleFile :: Either FilePath FilePath -- Left = tx body file, Right = tx bundle file
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
    <$> flagsParser
    <*> signingKeyFileParser
    <*> asum
      [ Left <$> txBodyFileParser
      , Right <$> txBundleFileParser
      ]
    <*> outFileParser

flagsParser :: Parser (BitSet Flags)
flagsParser =
  witherMap
    BS.fromList
    id
    [ quietParser
    , yesParser
    ]

quietParser :: Parser (Maybe Flags)
quietParser =
  optional $
    flag' Quiet $
      fold
        [ long "quiet"
        , short 'q'
        , help "Do not print summary output (implies -y)"
        ]

yesParser :: Parser (Maybe Flags)
yesParser =
  optional $
    flag' Yes $
      fold
        [ long "yes"
        , short 'y'
        , help "Automatically confirm all prompts"
        ]

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

txBundleFileParser :: Parser FilePath
txBundleFileParser =
  strOption $
    fold
      [ long "tx-bundle-file"
      , metavar "FILEPATH"
      , help "The file containing the transaction bundle to sign."
      , action "file"
      ]

outFileParser :: Parser FilePath
outFileParser =
  strOption $
    fold
      [ long "out-file"
      , short 'o'
      , metavar "FILEPATH"
      , help "A file path to save the transaction witness or witness bundle file."
      , action "file"
      ]

printIndented :: BitSet Flags -> String -> IO ()
printIndented flags s = unless (BS.member Quiet flags) $ putStrLn $ "  " <> s

printStyled :: BitSet Flags -> Maybe Color -> Bool -> String -> String -> IO ()
printStyled flags mColor bold label val
  | BS.member Quiet flags = pure ()
  | otherwise = do
      putStr "  "
      when bold $ setSGR [SetConsoleIntensity BoldIntensity]
      putStr label
      when bold $ setSGR [Reset]
      case mColor of
        Just c -> withColor c $ putStrLn val
        Nothing -> putStrLn val

withColor :: Color -> IO () -> IO ()
withColor color act = do
  setSGR [SetColor Foreground Vivid color]
  act
  setSGR [Reset]

printTitle :: BitSet Flags -> String -> IO ()
printTitle flags s = unless (BS.member Quiet flags) $ do
  putStrLn ""
  putStrLn s

dieIndented :: String -> IO a
dieIndented s = die $ "  " <> s

classifyTx :: BitSet Flags -> TxBody ConwayEra -> IO TxClassification
classifyTx flags (ShelleyTxBody ShelleyBasedEraConway _ _ scriptData _ _) = do
  printTitle flags "Checking transaction purpose..."
  case scriptData of
    TxBodyNoScriptData -> dieIndented "Transaction does not execute any scripts."
    TxBodyScriptData AlonzoEraOnwardsConway _ (Redeemers redeemers) ->
      case spendingRedeemers redeemers of
        [] -> dieIndented "Transaction does not spend any script outputs."
        [(L.Data rdmr, _)] -> case rdmr of
          Constr 0 [fromData -> Just (HotCommitteeCredential hotCred)] -> do
            printIndented flags "Hot credential authorization transaction."
            case hotCred of
              ScriptCredential hash ->
                printIndented flags $ "Hot credential script hash: " <> show hash <> "."
              PubKeyCredential hash -> do
                printIndented flags $ "Hot credential pub key hash: " <> show hash <> "."
                printIndented
                  flags
                  "WARNING: This is unusual, normally a script credential should be authorized."
            pure $ ColdTx $ AuthorizeHot $ HotCommitteeCredential hotCred
          Constr 1 [] -> do
            printIndented flags "Constitutional committee resignation transaction."
            pure $ ColdTx ResignCold
          Constr 2 [fromData -> Just Identity{..}] -> do
            printIndented flags "Delegate resignation transaction."
            printIndented flags $ "Resignee certificate hash: " <> show certificateHash
            printIndented flags $ "Resignee public key hash: " <> show pubKeyHash
            pure $ ColdTx $ ResignDelegation Identity{..}
          Constr 3 [fromData -> Just Identity{..}] -> do
            printIndented flags "Membership resignation transaction."
            printIndented flags $ "Resignee certificate hash: " <> show certificateHash
            printIndented flags $ "Resignee public key hash: " <> show pubKeyHash
            pure $ ColdTx $ ResignMembership Identity{..}
          Constr 4 [] -> do
            printIndented flags "Cold credential key rotation transaction."
            pure $ ColdTx RotateCold
          Constr 5 [] -> do
            printIndented flags "Cold NFT burn transaction."
            pure $ ColdTx BurnCold
          Constr 6 [fromData -> Just scriptHash] -> do
            printIndented flags "Cold NFT upgrade transaction."
            printIndented flags $ "New lock script hash: " <> show scriptHash
            pure $ ColdTx $ UpgradeCold scriptHash
          Constr 7 [] -> do
            printIndented flags "Vote transaction."
            pure $ HotTx Vote
          Constr 8 [fromData -> Just Identity{..}] -> do
            printIndented flags "Voter resignation transaction."
            printIndented flags $ "Resignee certificate hash: " <> show certificateHash
            printIndented flags $ "Resignee public key hash: " <> show pubKeyHash
            pure $ HotTx $ ResignVoting Identity{..}
          Constr 9 [] -> do
            printIndented flags "Hot credential key rotation transaction."
            pure $ HotTx RotateHot
          Constr 10 [] -> do
            printIndented flags "Hot NFT burn transaction."
            pure $ ColdTx BurnCold
          Constr 11 [fromData -> Just scriptHash] -> do
            printIndented flags "Hot NFT upgrade transaction."
            printIndented flags $ "New lock script hash: " <> show scriptHash
            pure $ ColdTx $ UpgradeCold scriptHash
          _ -> do
            die "Transaction has invalid redeemer datum."
        _ -> do
          die "Transaction spends multiple script outputs."

data TxClassification
  = ColdTx ColdLockRedeemer
  | HotTx HotLockRedeemer
  deriving (Show, Eq)

summarizeCertificates
  :: BitSet Flags -> TxBody ConwayEra -> TxClassification -> IO Bool
summarizeCertificates flags (TxBody TxBodyContent{..}) classification = do
  printTitle flags "Check transaction certificates..."
  let certificatesExpected = case classification of
        ColdTx AuthorizeHot{} -> True
        ColdTx ResignCold -> True
        _ -> False
  case txCertificates of
    TxCertificatesNone
      | certificatesExpected -> do
          dieIndented "No certificates found"
      | otherwise -> do
          printIndented flags "No certificates found, as expected"
          pure False
    TxCertificates _ [] _
      | certificatesExpected -> do
          dieIndented "No certificates found"
      | otherwise -> do
          printIndented flags "No certificates found, as expected"
          pure False
    TxCertificates _ [certificate] _
      | certificatesExpected -> do
          printCertificate flags certificate
          pure True
      | otherwise -> do
          printIndented flags "Unexpected certificate found:"
          printCertificate flags certificate
          exitFailure
    TxCertificates{} -> do
      dieIndented "Unexpected multiple certificates found"

printCertificate :: BitSet Flags -> Certificate ConwayEra -> IO ()
printCertificate _ (ShelleyRelatedCertificate era _) = case era of {}
printCertificate flags (ConwayCertificate _ (ConwayTxCertDeleg delegCert)) = do
  printIndented flags "Unexpected delegation certificate found"
  dieIndented $ show delegCert
printCertificate flags (ConwayCertificate _ (ConwayTxCertPool poolCert)) = do
  printIndented flags "Unexpected pool certificate found"
  dieIndented $ show poolCert
printCertificate flags (ConwayCertificate _ (ConwayTxCertGov govCert)) =
  case govCert of
    ConwayAuthCommitteeHotKey coldCredential hotCredential -> do
      printIndented flags "Authorize committee hot credential certificate found"
      printIndented flags $ "Cold credential: " <> show coldCredential
      printIndented flags $ "Hot credential: " <> show hotCredential
    ConwayResignCommitteeColdKey coldCredential sAnchor -> do
      printIndented
        flags
        "Constitutional committee cold credential resignation certificate found"
      printIndented flags $ "Cold credential: " <> show coldCredential
      case sAnchor of
        SNothing -> do
          printIndented flags "WARNING: No anchor found"
        SJust anchor -> printIndented flags $ "Anchor " <> show anchor
    _ -> do
      printIndented flags "Unexpected gov certificate found"
      dieIndented $ show govCert

summarizeVotes
  :: BitSet Flags -> TxBody ConwayEra -> TxClassification -> IO Bool
summarizeVotes flags (TxBody TxBodyContent{..}) classification = do
  printTitle flags "Check transaction votes..."
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
                  case voter of
                    L.CommitteeVoter (L.ScriptHashObj (L.ScriptHash h)) ->
                      printIndented flags $
                        "Voting as hot credential: "
                          <> show h
                    _ ->
                      dieIndented $ "Unexpected voter type: " <> show voter
                  traverse_ (uncurry $ summarizeVote flags) (Map.toList votes)
                  pure True
            _ -> do
              dieIndented "Votes cast by multiple voters"
    _ ->
      case txVotingProcedures of
        Nothing -> do
          printIndented flags "No votes cast, as expected"
          pure False
        Just (Featured ConwayEraOnwardsConway TxVotingProceduresNone) -> do
          printIndented flags "No votes cast, as expected"
          pure False
        Just
          ( Featured
              ConwayEraOnwardsConway
              (TxVotingProcedures (VotingProcedures voters) _)
            )
            | Map.null voters -> do
                printIndented flags "No votes cast, as expected"
                pure False
            | otherwise -> do
                dieIndented "Transaction casts votes when not allowed to"

summarizeVote
  :: BitSet Flags
  -> GovActionId L.StandardCrypto
  -> VotingProcedure (L.ConwayEra StandardCrypto)
  -> IO ()
summarizeVote flags govActionId (VotingProcedure vote mAnchor) = do
  printIndented flags "Vote cast:"
  case vote of
    L.VoteYes -> printStyled flags (Just Green) True "  Vote: " "Yes"
    L.VoteNo -> printStyled flags (Just Red) True "  Vote: " "No"
    L.Abstain -> printStyled flags (Just Magenta) True "  Vote: " "Abstain"
  let govactionText = govActionIdToText govActionId
  printStyled
    flags
    (Just Yellow)
    True
    "On governance action: "
    (T.unpack govactionText)
  case mAnchor of
    SNothing -> printIndented flags "WARNING: No rationale file anchor included in vote"
    SJust anchor -> do
      printIndented flags "Rationale:"
      printStyled
        flags
        (Just Blue)
        True
        "  Anchor url: "
        (show $ L.urlToText $ L.anchorUrl anchor)
      printStyled
        flags
        (Just Blue)
        True
        "  Anchor hash: "
        (show $ L.extractHash $ L.anchorDataHash anchor)

summarizeOutputs
  :: BitSet Flags -> Hash PaymentKey -> TxClassification -> TxBody ConwayEra -> IO ()
summarizeOutputs flags myKey classification (TxBody TxBodyContent{..}) =
  for_ (zip @Int [0 ..] txOuts) \(ix, txOut) -> do
    printTitle flags $ "Check transaction output #" <> show ix <> "..."
    summarizeOutput flags myKey classification txOut
    promptToProceed flags "Is this output OK?"

summarizeOutput
  :: BitSet Flags
  -> Hash PaymentKey
  -> TxClassification
  -> TxOut CtxTx ConwayEra
  -> IO ()
summarizeOutput flags myKey classification (TxOut address outValue outDatum _refScript) = do
  let val = txOutValueToValue outValue
  printIndented flags $ "Send to address " <> T.unpack (serialiseAddress address)
  printIndented flags $ show (L.unCoin $ selectLovelace val) <> " Lovelace"
  for_ (valueToList val) \case
    (AdaAssetId, _) -> pure ()
    (AssetId p n, Quantity q) ->
      printIndented flags $
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
      summarizeDatum flags myKey classification datum
    TxOutDatumInline _ datum ->
      summarizeDatum flags myKey classification datum

summarizeDatum
  :: BitSet Flags
  -> Hash PaymentKey
  -> TxClassification
  -> HashableScriptData
  -> IO ()
summarizeDatum flags myKey classification datum = do
  let plutusDatum = toPlutusData $ getScriptData datum
  case fromData plutusDatum of
    Just ColdLockDatum{..} -> do
      printIndented flags "Cold NFT datum found"
      case classification of
        ColdTx RotateCold -> do
          printIndented flags "New membership keys:"
          for_ membershipUsers $ summarizeIdentity flags myKey
          printIndented flags "New delegation keys:"
          for_ delegationUsers $ summarizeIdentity flags myKey
        ColdTx _ -> pure ()
        HotTx _ -> do
          dieIndented "Not expected in a hot credential transaction"
    Nothing -> case fromData plutusDatum of
      Just HotLockDatum{..} -> do
        printIndented flags "Hot NFT datum found"
        case classification of
          HotTx RotateHot -> do
            printIndented flags "New voting keys:"
            for_ votingUsers $ summarizeIdentity flags myKey
          HotTx _ -> pure ()
          ColdTx _ -> do
            dieIndented "Not expected in a cold credential transaction"
      Nothing -> do
        printIndented flags "Unrecognized datum in output"
        dieIndented $ show datum

summarizeIdentity :: BitSet Flags -> Hash PaymentKey -> Identity -> IO ()
summarizeIdentity flags myKey Identity{..} = do
  let pubKeyString = show pubKeyHash
  let canSign = pubKeyString == T.unpack (serialiseToRawBytesHexText myKey)
  printIndented flags $
    "- public key hash: "
      <> show pubKeyHash
      <> if canSign
        then " (this is your key)"
        else ""
  printIndented flags $ "  certificate hash: " <> show certificateHash

checkExtraTxBodyFields
  :: TxClassification -> BitSet Flags -> TxBody ConwayEra -> IO ()
checkExtraTxBodyFields classification flags (TxBody TxBodyContent{..}) = do
  printTitle flags "Check extra tx body fields..."
  case txWithdrawals of
    TxWithdrawalsNone -> pure ()
    TxWithdrawals _ [] -> pure ()
    TxWithdrawals _ _ -> do
      printIndented
        flags
        "WARNING: Transaction unexpectedly withdraws staking rewards."
      promptToProceed flags "Are you OK with this?"
  case txMintValue of
    TxMintNone -> pure ()
    TxMintValue _ val _
      | val == mempty
          || classification == ColdTx BurnCold
          || classification == HotTx BurnHot ->
          pure ()
      | otherwise -> do
          printIndented flags "WARNING: Transaction unexpectedly mints or burns tokens."
          promptToProceed flags "Are you OK with this?"
  case txProposalProcedures of
    Nothing -> pure ()
    Just (Featured _ TxProposalProceduresNone) -> pure ()
    Just (Featured _ (TxProposalProcedures proposalProcedures _))
      | proposalProcedures == mempty -> pure ()
      | otherwise -> do
          printIndented
            flags
            "WARNING Transaction unexpectedly submits governance actions."
          promptToProceed flags "Are you OK with this?"

summarizeSignatories
  :: BitSet Flags
  -> Hash PaymentKey
  -> Either (TxBody ConwayEra) (TxBundle ConwayEra)
  -> IO ()
summarizeSignatories flags myKey bodyOrBundle = do
  printTitle flags "Check transaction signatories..."
  canSign <- case bodyOrBundle of
    Left (TxBody TxBodyContent{txExtraKeyWits}) -> case txExtraKeyWits of
      TxExtraKeyWitnessesNone -> dieIndented "This transaction requires no signatures"
      TxExtraKeyWitnesses _ signatories -> do
        or <$> for signatories \pkh -> do
          let canSign = pkh == myKey
          printIndented flags $
            "Requires signature from "
              <> T.unpack (serialiseToRawBytesHexText pkh)
              <> if canSign
                then " (you can sign)"
                else ""
          pure canSign
    Right TxBundle{..} -> do
      let canSign = case U.findIndex (== myKey) tbSigTab of
            Nothing -> False
            Just mySigIndex -> U.any ((== mySigIndex) . fromIntegral . gmSig) tbGroupMemTab
      when canSign $ printIndented flags "You can sign"
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
    traverse_ (putStrLn . ("- " <>) . fromString . unTextEnvelopeType) expected
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
