module Commands (
  Command,
  commandParser,
  runCommand,
) where

import Cardano.Api (
  AsType (AsPaymentKey, AsSigningKey),
  SerialiseAsRawBytes (..),
  readFileTextEnvelope,
  writeFileTextEnvelope,
 )
import Cardano.Api.Byron (File (..))
import Commands.Authorize (
  AuthorizeCommand,
  authorizeCommandParser,
  runAuthorizeCommand,
 )
import Commands.BurnCold (
  BurnColdCommand,
  burnColdCommandParser,
  runBurnColdCommand,
 )
import Commands.BurnHot (
  BurnHotCommand,
  burnHotCommandParser,
  runBurnHotCommand,
 )
import Commands.InitCold (
  InitColdCommand,
  initColdCommandParser,
  runInitColdCommand,
 )
import Commands.InitHot (
  InitHotCommand,
  initHotCommandParser,
  runInitHotCommand,
 )
import Commands.Resign (ResignCommand, resignCommandParser, runResignCommand)
import Commands.ResignDelegation (
  ResignDelegationCommand,
  resignDelegationCommandParser,
  runResignDelegationCommand,
 )
import Commands.ResignMembership (
  ResignMembershipCommand,
  resignMembershipCommandParser,
  runResignMembershipCommand,
 )
import Commands.ResignVoting (
  ResignVotingCommand,
  resignVotingCommandParser,
  runResignVotingCommand,
 )
import Commands.RotateCold (
  RotateColdCommand,
  rotateColdCommandParser,
  runRotateColdCommand,
 )
import Commands.RotateHot (
  RotateHotCommand,
  rotateHotCommandParser,
  runRotateHotCommand,
 )
import Commands.UpgradeCold (
  UpgradeColdCommand,
  runUpgradeColdCommand,
  upgradeColdCommandParser,
 )
import Commands.UpgradeHot (
  UpgradeHotCommand,
  runUpgradeHotCommand,
  upgradeHotCommandParser,
 )
import Commands.Vote (VoteCommand, runVoteCommand, voteCommandParser)
import Control.Monad (unless)
import CredentialManager.Api (
  Identity (Identity, pubKeyHash),
  parseIdentityFromPEMBytes,
 )
import Crypto.Error (eitherCryptoError)
import qualified Crypto.PubKey.Ed25519 as Crypton
import Data.ASN1.BinaryEncoding (BER (..), DER (DER))
import Data.ASN1.Encoding (decodeASN1', encodeASN1')
import Data.ASN1.Types (ASN1Object (..))
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (..))
import Data.PEM (PEM (..), pemParseBS, pemWriteBS)
import Data.X509 (PrivKey (..))
import Options.Applicative (
  InfoMod,
  Parser,
  action,
  command,
  help,
  hsubparser,
  info,
  metavar,
  progDesc,
  strArgument,
 )
import PlutusLedgerApi.V1 (
  LedgerBytes (LedgerBytes),
  PubKeyHash (getPubKeyHash),
 )
import System.Exit (die)

data Command
  = InitCold InitColdCommand
  | Authorize AuthorizeCommand
  | Resign ResignCommand
  | ResignMembership ResignMembershipCommand
  | ResignDelegation ResignDelegationCommand
  | RotateCold RotateColdCommand
  | BurnCold BurnColdCommand
  | UpgradeCold UpgradeColdCommand
  | InitHot InitHotCommand
  | Vote VoteCommand
  | ResignVoting ResignVotingCommand
  | RotateHot RotateHotCommand
  | BurnHot BurnHotCommand
  | UpgradeHot UpgradeHotCommand
  | FromPem FilePath FilePath
  | ToPem FilePath FilePath
  | ExtractPubKeyHash FilePath

-- Parsers

commandParser :: Parser Command
commandParser =
  hsubparser $
    fold
      [ command "init-cold" $ InitCold <$> initColdCommandParser
      , command "init-hot" $ InitHot <$> initHotCommandParser
      , command "authorize" $ Authorize <$> authorizeCommandParser
      , command "vote" $ Vote <$> voteCommandParser
      , command "resign-committee" $ Resign <$> resignCommandParser
      , command "resign-membership" $ ResignMembership <$> resignMembershipCommandParser
      , command "resign-delegation" $ ResignDelegation <$> resignDelegationCommandParser
      , command "resign-voting" $ ResignVoting <$> resignVotingCommandParser
      , command "rotate-cold" $ RotateCold <$> rotateColdCommandParser
      , command "rotate-hot" $ RotateHot <$> rotateHotCommandParser
      , command "burn-cold" $ BurnCold <$> burnColdCommandParser
      , command "burn-hot" $ BurnHot <$> burnHotCommandParser
      , command "upgrade-cold" $ UpgradeCold <$> upgradeColdCommandParser
      , command "upgrade-hot" $ UpgradeHot <$> upgradeHotCommandParser
      , command "from-pem" $
          flip info fromPemDesc $
            FromPem <$> pemInFileParser <*> textEnvelopeOutFileParser
      , command "to-pem" $
          flip info toPemDesc $
            ToPem <$> textEnvelopeInFileParser <*> pemOutFileParser
      , command "extract-pub-key-hash" $
          flip info extractPubKeyHashDesc $
            ExtractPubKeyHash <$> certInFileParser
      ]

extractPubKeyHashDesc :: InfoMod Command
extractPubKeyHashDesc = progDesc "Extract a pub key hash from a certificate PEM file."

toPemDesc :: InfoMod Command
toPemDesc = progDesc "Convert a cardano-cli text envelope file to an OpenSSL PEM file."

fromPemDesc :: InfoMod Command
fromPemDesc = progDesc "Convert an OpenSSL PEM text envelope file to a cardano-cli file."

pemOutFileParser :: Parser FilePath
pemOutFileParser =
  strArgument $
    fold
      [ metavar "FILE_PATH"
      , help "An OpenSSL PRIVATE KEY PEM file."
      , action "file"
      ]

textEnvelopeInFileParser :: Parser FilePath
textEnvelopeInFileParser =
  strArgument $
    fold
      [ metavar "FILE_PATH"
      , help
          "A cardano-cli text envelope file containing a PaymentSigningKeyShelley_ed25519."
      , action "file"
      ]

textEnvelopeOutFileParser :: Parser FilePath
textEnvelopeOutFileParser =
  strArgument $
    fold
      [ metavar "FILE_PATH"
      , help "A file path to write the OpenSSL PRIVATE KEY PEM file to."
      , action "file"
      ]

pemInFileParser :: Parser FilePath
pemInFileParser =
  strArgument $
    fold
      [ metavar "FILE_PATH"
      , help
          "A file path to write the cardano-cli text envelope PaymentSigningKeyShelley_ed25519 file to."
      , action "file"
      ]

certInFileParser :: Parser FilePath
certInFileParser =
  strArgument $
    fold
      [ metavar "FILE_PATH"
      , help "An X.509 certificate from which to extract the public key hash."
      , action "file"
      ]

-- Implementations

runCommand :: Command -> IO ()
runCommand = \case
  InitCold cmd -> runInitColdCommand cmd
  Authorize cmd -> runAuthorizeCommand cmd
  Resign cmd -> runResignCommand cmd
  ResignMembership cmd -> runResignMembershipCommand cmd
  ResignDelegation cmd -> runResignDelegationCommand cmd
  RotateCold cmd -> runRotateColdCommand cmd
  BurnCold cmd -> runBurnColdCommand cmd
  InitHot cmd -> runInitHotCommand cmd
  Vote cmd -> runVoteCommand cmd
  ResignVoting cmd -> runResignVotingCommand cmd
  RotateHot cmd -> runRotateHotCommand cmd
  BurnHot cmd -> runBurnHotCommand cmd
  UpgradeHot cmd -> runUpgradeHotCommand cmd
  UpgradeCold cmd -> runUpgradeColdCommand cmd
  ToPem src dest -> runToPEM src dest
  FromPem src dest -> runFromPEM src dest
  ExtractPubKeyHash certFile -> runExtractPubKeyHash certFile

runToPEM :: FilePath -> FilePath -> IO ()
runToPEM src dest = do
  signingKeyResult <- readFileTextEnvelope (AsSigningKey AsPaymentKey) (File src)
  signingKey <-
    assertRight signingKeyResult $ ("Failed to read input file: " <>) . show
  secretKey <-
    assertRight
      (eitherCryptoError $ Crypton.secretKey $ serialiseToRawBytes signingKey)
      $ ( "Failed to convert from Cardano.Api.SigningKey Cardano.Api.PaymentKey to crypton SecretKey: "
            <>
        )
        . show
  let asn1 = toASN1 (PrivKeyEd25519 secretKey) []
  let pemContent = encodeASN1' DER asn1
  let pem =
        PEM
          { pemName = "PRIVATE KEY"
          , pemHeader = []
          , pemContent
          }
  let privateKeyPemBytes = pemWriteBS pem
  BS.writeFile dest privateKeyPemBytes

assertRight :: Either a b -> (a -> String) -> IO b
assertRight (Left a) f = die $ f a
assertRight (Right b) _ = pure b

runFromPEM :: FilePath -> FilePath -> IO ()
runFromPEM src dest = do
  srcBytes <- BS.readFile src
  pems <-
    assertRight (pemParseBS srcBytes) ("Failed to decode input PEM file: " <>)
  PEM{..} <- case pems of
    [pem] -> pure pem
    [] -> die "Empty PEM file"
    _ -> die "More than one PEM entry"
  unless (pemName == "PRIVATE KEY") $ die $ "Unexpected PEM name: " <> pemName
  asn1 <-
    assertRight (decodeASN1' BER pemContent) $
      ("Failed to decode PEM file contents as ASN1 stream: " <>) . show
  (pk, stream) <-
    assertRight (fromASN1 asn1) ("Failed to decode ASN1 stream from PEM file: " <>)
  unless (null stream) $ die "ASN1 stream end expected"
  secretKey <- case pk of
    PrivKeyEd25519 key -> pure key
    PrivKeyRSA _ -> die "RSA private keys not supported"
    PrivKeyDSA _ -> die "DSA private keys not supported"
    PrivKeyEC _ -> die "EC private keys not supported"
    PrivKeyX25519 _ -> die "X25519 private keys not supported"
    PrivKeyX448 _ -> die "X448 private keys not supported"
    PrivKeyEd448 _ -> die "Ed448 private keys not supported"
  signingKey <-
    assertRight
      (deserialiseFromRawBytes (AsSigningKey AsPaymentKey) $ convert secretKey)
      $ ( "Failed to convert from crypton SecretKey to Cardano.Api.SigningKey Cardano.Api.PaymentKey: "
            <>
        )
        . show
  flip assertRight (("Failed to write output file: " <>) . show)
    =<< writeFileTextEnvelope (File dest) (Just "Payment Signing Key") signingKey

runExtractPubKeyHash :: FilePath -> IO ()
runExtractPubKeyHash certFile = do
  srcBytes <- BS.readFile certFile
  let parseResult = parseIdentityFromPEMBytes srcBytes
  case parseResult of
    Left err -> die err
    Right Identity{..} -> print $ LedgerBytes $ getPubKeyHash pubKeyHash
