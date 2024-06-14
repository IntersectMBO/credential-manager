module Commands.Common where

import Cardano.Api (
  Address,
  AsType (..),
  AssetName,
  Certificate,
  ConwayEra,
  File (..),
  FromSomeType (..),
  IsPlutusScriptLanguage,
  Key (..),
  NetworkId (..),
  NetworkMagic (..),
  PlutusScriptV3,
  PlutusScriptVersion (..),
  PolicyId,
  Script (..),
  ScriptHash,
  SerialiseAsBech32,
  SerialiseAsRawBytes (..),
  SerialiseAsRawBytesError (unSerialiseAsRawBytesError),
  ShelleyAddr,
  StakeAddressReference (..),
  TxIn (..),
  TxIx (..),
  UTxO (..),
  Value,
  hashScript,
  plutusScriptVersion,
  readFileTextEnvelope,
  readFileTextEnvelopeAnyOf,
  renderValue,
  serialiseToBech32,
  serialiseToRawBytesHexText,
  unsafeHashableScriptData,
  writeFileTextEnvelope,
 )
import Cardano.Api.Ledger (
  AnchorData,
  SafeHash,
  StandardCrypto,
  Url,
  hashFromBytes,
  textToUrl,
  unsafeMakeSafeHash,
 )
import Cardano.Api.Shelley (
  StakeCredential (..),
  fromPlutusData,
  scriptDataToJsonDetailedSchema,
 )
import qualified Cardano.Api.Shelley as Shelley
import Control.Applicative ((<|>))
import CredentialManager.Api (Identity, readIdentityFromPEMFile)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16Untyped)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (Foldable (..), asum)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Options.Applicative (
  Mod,
  OptionFields,
  Parser,
  ReadM,
  action,
  eitherReader,
  flag,
  flag',
  help,
  long,
  metavar,
  option,
  readerError,
  short,
  str,
  strOption,
 )
import PlutusLedgerApi.V2 (TxOutRef (TxOutRef))
import qualified PlutusLedgerApi.V2 as PV2
import PlutusTx (ToData, toData)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))
import Text.Read (readEither)

data StakeCredentialFile = StakeKey FilePath | StakeScript FilePath

networkIdParser :: Parser NetworkId
networkIdParser =
  asum
    [ flag' Mainnet $
        fold
          [ long "mainnet"
          , help "Build a mainnet script address"
          ]
    , -- The network magic is unimportant for addresses.
      flag' (Testnet $ NetworkMagic 1) $
        fold
          [ long "testnet"
          , help "Build a testnet script address"
          ]
    ]

debugParser :: Parser Bool
debugParser =
  flag False True $
    fold
      [ long "debug"
      , short 'd'
      , help "Compile scripts in debug mode"
      ]

assetNameParser :: Mod OptionFields AssetName -> Parser AssetName
assetNameParser = option readAssetName . (<> metavar "ASSET_NAME")

policyIdParser :: Mod OptionFields PolicyId -> Parser PolicyId
policyIdParser = option readPolicyId . (<> metavar "POLICY_ID")

seedInputParser :: Parser TxIn
seedInputParser =
  option readTxIn $
    fold
      [ long "seed-input"
      , help "The tx in to consume to determine the token name of the NFT"
      , metavar "TX_ID#TX_IX"
      ]

utxoFileParser :: Parser FilePath
utxoFileParser =
  strOption $
    fold
      [ long "utxo-file"
      , short 'u'
      , metavar "FILE_PATH"
      , help
          "A relative path to a JSON file containing the unspent transaction output holding the NFT. Obtain with cardano-cli query utxo --output-json"
      , action "file"
      ]

coldCredentialScriptFileParser :: Parser FilePath
coldCredentialScriptFileParser =
  strOption $
    fold
      [ long "cold-credential-script-file"
      , metavar "FILE_PATH"
      , help "A relative path to the compiled cold credential script file."
      , action "file"
      ]

hotCredentialScriptFileParser :: Parser FilePath
hotCredentialScriptFileParser =
  strOption $
    fold
      [ long "hot-credential-script-file"
      , metavar "FILE_PATH"
      , help "A relative path to the compiled hot credential script file."
      , action "file"
      ]

newScriptFileParser :: Parser FilePath
newScriptFileParser =
  strOption $
    fold
      [ long "new-script-file"
      , metavar "FILE_PATH"
      , help "The relative path of a file containing the script to send the NFT to."
      , action "file"
      ]

newScriptHashParser :: Parser ScriptHash
newScriptHashParser =
  strOption $
    fold
      [ long "new-script-hash"
      , metavar "HEX"
      , help "The Blake2b-224 hash of the script to send the NFT to, in hex."
      ]

newScriptParser :: Parser (Either ScriptHash FilePath)
newScriptParser = Left <$> newScriptHashParser <|> Right <$> newScriptFileParser

outDirParser :: Parser FilePath
outDirParser =
  strOption $
    fold
      [ long "out-dir"
      , short 'o'
      , metavar "DIRECTORY"
      , help
          "A relative path to the directory where the output assets should be written."
      , action "directory"
      ]

stakeCredentialFileParser :: Parser StakeCredentialFile
stakeCredentialFileParser =
  asum
    [ fmap StakeKey $
        strOption $
          fold
            [ long "stake-verification-key-file"
            , metavar "FILE_PATH"
            , help
                "A relative path to the stake verification key to build the script address with."
            , action "file"
            ]
    , fmap StakeScript $
        strOption $
          fold
            [ long "stake-script-file"
            , metavar "FILE_PATH"
            , help "A relative path to the stake script to build the script address with."
            , action "file"
            ]
    ]

membershipCertParser :: Parser FilePath
membershipCertParser =
  strOption $
    fold
      [ long "membership-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the certificate PEM file of a membership user."
      , action "file"
      ]

delegationCertParser :: Parser FilePath
delegationCertParser =
  strOption $
    fold
      [ long "delegation-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the certificate PEM file of a delegation user."
      , action "file"
      ]

votingCertParser :: Parser FilePath
votingCertParser =
  strOption $
    fold
      [ long "voting-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the certificate PEM file of a voting user."
      , action "file"
      ]

readPolicyId :: ReadM PolicyId
readPolicyId = do
  bytes <- readBase16
  either
    (readerError . unSerialiseAsRawBytesError)
    pure
    $ deserialiseFromRawBytes AsPolicyId bytes

readAssetName :: ReadM AssetName
readAssetName = do
  bytes <- readBase16
  either
    (readerError . unSerialiseAsRawBytesError)
    pure
    $ deserialiseFromRawBytes AsAssetName bytes

readTxIn :: ReadM TxIn
readTxIn = eitherReader \raw -> do
  let (txIdStr, rest) = splitAt 64 raw
  txIdBytes <-
    first (const "Invalid hexadecimal text")
      . decodeBase16Untyped
      . T.encodeUtf8
      $ T.pack txIdStr
  txId <-
    first unSerialiseAsRawBytesError $ deserialiseFromRawBytes AsTxId txIdBytes
  case rest of
    '#' : txIxStr -> do
      txIxWord <- readEither txIxStr
      pure $ TxIn txId $ TxIx txIxWord
    _ -> Left "Expected \"#<TX_IX>\""

readBase16 :: ReadM ByteString
readBase16 =
  eitherReader $
    first (const "Invalid hexadecimal text")
      . decodeBase16Untyped
      . T.encodeUtf8
      . T.pack

metadataUrlParser :: Mod OptionFields Url -> Parser Url
metadataUrlParser info =
  option readUrl $
    fold
      [ long "metadata-url"
      , metavar "URL"
      ]
      <> info

readUrl :: ReadM Url
readUrl = do
  text <- str
  textToUrl 128 text

metadataHashParser :: Parser (SafeHash StandardCrypto AnchorData)
metadataHashParser =
  option readSafeHash $
    fold
      [ long "metadata-hash"
      , metavar "HASH"
      , help "Hash of the anchor data as hexadecimal text"
      ]

readSafeHash :: ReadM (SafeHash StandardCrypto a)
readSafeHash = do
  bytes <- readBase16
  case hashFromBytes bytes of
    Nothing -> readerError "Unable to read hash"
    Just hash -> pure $ unsafeMakeSafeHash hash

readIdentityFromPEMFile' :: FilePath -> IO Identity
readIdentityFromPEMFile' file =
  readIdentityFromPEMFile file >>= \case
    Left err -> error $ file <> ": " <> err
    Right a -> pure a

readStakeAddressFile :: StakeCredentialFile -> IO StakeAddressReference
readStakeAddressFile =
  fmap StakeAddressByValue . \case
    StakeKey file -> do
      keyResult <- readFileTextEnvelope (AsVerificationKey AsStakeKey) $ File file
      case keyResult of
        Left err -> do
          error $ "Failed to read stake verification key file: " <> show err
        Right key -> pure $ StakeCredentialByKey $ verificationKeyHash key
    StakeScript file -> do
      scriptHashResult <-
        readFileTextEnvelopeAnyOf
          [ FromSomeType (AsPlutusScript AsPlutusScriptV3) $
              hashScript . PlutusScript PlutusScriptV3
          , FromSomeType (AsPlutusScript AsPlutusScriptV3) $
              hashScript . PlutusScript PlutusScriptV3
          , FromSomeType (AsPlutusScript AsPlutusScriptV3) $
              hashScript . PlutusScript PlutusScriptV3
          ]
          (File file)
      case scriptHashResult of
        Left err -> do
          error $ "Failed to read stake script file: " <> show err
        Right hash -> pure $ StakeCredentialByScript hash

readFilePlutusV3Script :: FilePath -> IO (Script PlutusScriptV3)
readFilePlutusV3Script path = do
  result <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File path)
  case result of
    Left err -> error $ "Failed to read script file " <> path <> ": " <> show err
    Right script -> pure $ PlutusScript PlutusScriptV3 script

readFileUTxO :: FilePath -> IO (UTxO ConwayEra)
readFileUTxO path = do
  result <- eitherDecodeFileStrict @(UTxO ConwayEra) path
  case result of
    Left err -> do error $ "Failed to read utxo file: " <> show err
    Right u -> pure u

writeCertificateToFile
  :: FilePath -> FilePath -> Certificate ConwayEra -> IO ()
writeCertificateToFile dir file certificate = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  either (error . show) pure
    =<< writeFileTextEnvelope (File path) Nothing certificate

runCommand
  :: (inputs -> Either err outputs)
  -> inputs
  -> (err -> String)
  -> IO outputs
runCommand cmd inputs renderError = either (die . renderError) pure $ cmd inputs

writeVoteToFile
  :: FilePath
  -> FilePath
  -> Shelley.VotingProcedures ConwayEra
  -> IO ()
writeVoteToFile dir file votingProcedures = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  either (error . show) pure
    =<< writeFileTextEnvelope (File path) Nothing votingProcedures

writeScriptToFile
  :: forall lang
   . (IsPlutusScriptLanguage lang)
  => FilePath
  -> FilePath
  -> Script lang
  -> IO ()
writeScriptToFile dir file (PlutusScript _ script) = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  either (error . show) pure
    =<< writeFileTextEnvelope (File path) Nothing script
writeScriptToFile _ _ SimpleScript{} = case plutusScriptVersion @lang of {}

writeHexBytesToFile
  :: (SerialiseAsRawBytes a) => FilePath -> FilePath -> a -> IO ()
writeHexBytesToFile dir file a = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  T.writeFile path $ serialiseToRawBytesHexText a

writeBech32ToFile :: (SerialiseAsBech32 a) => FilePath -> FilePath -> a -> IO ()
writeBech32ToFile dir file a = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  T.writeFile path $ serialiseToBech32 a

writePlutusDataToFile :: (ToData a) => FilePath -> FilePath -> a -> IO ()
writePlutusDataToFile dir file a = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  LBS.writeFile path $
    encodePretty $
      scriptDataToJsonDetailedSchema $
        unsafeHashableScriptData $
          fromPlutusData $
            toData a

writeTxOutValueToFile
  :: FilePath -> FilePath -> Address ShelleyAddr -> Value -> IO ()
writeTxOutValueToFile dir file address value = do
  createDirectoryIfMissing True dir
  let path = dir </> file
  T.writeFile path $
    fold
      [ serialiseToBech32 address
      , "+"
      , renderValue value
      ]

extractTxIn :: UTxO ConwayEra -> IO TxOutRef
extractTxIn (UTxO utxo) = case Map.keys utxo of
  [] -> die "Script UTxO is empty"
  [TxIn txId (TxIx txIx)] ->
    pure $
      TxOutRef
        (PV2.TxId $ PV2.toBuiltin $ serialiseToRawBytes txId)
        (toInteger txIx)
  _ -> die "Script UTxO has more than one output"
