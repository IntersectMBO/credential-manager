module Commands.InitColdNFT where

import Cardano.Api (
  AsType (..),
  File (..),
  FromSomeType (..),
  Key (..),
  PaymentCredential (..),
  PlutusScriptVersion (..),
  Script (..),
  SerialiseAsBech32,
  SerialiseAsRawBytes (..),
  StakeAddressReference (..),
  hashScript,
  makeShelleyAddress,
  readFileTextEnvelope,
  readFileTextEnvelopeAnyOf,
  serialiseToBech32,
  unsafeHashableScriptData,
 )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (
  StakeCredential (..),
  fromPlutusData,
  scriptDataToJsonDetailedSchema,
 )
import Commands.InitColdCredential (
  scriptHashOutParser,
  scriptOutParser,
  writeHexBytesToFile,
  writeScriptToFile,
 )
import CredentialManager.Api (
  ColdLockDatum (..),
  Identity,
  readIdentityFromPEMFile,
 )
import qualified CredentialManager.Scripts as Scripts
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (Foldable (..), asum)
import qualified Data.Text.IO as T
import Options.Applicative (
  Alternative (some),
  InfoMod,
  Parser,
  ParserInfo,
  action,
  flag',
  help,
  info,
  long,
  metavar,
  optional,
  progDesc,
  strOption,
 )
import PlutusLedgerApi.V3 (
  ColdCommitteeCredential (..),
  Credential (..),
  ScriptHash (..),
  toBuiltin,
  toData,
 )

data NetworkType = Mainnet | Testnet

data StakeCredentialFile = StakeKey FilePath | StakeScript FilePath

data InitColdNFTCommand = InitColdNFTCommand
  { networkType :: NetworkType
  , coldCredentialScriptFile :: FilePath
  , caCertFile :: FilePath
  , membershipCertFiles :: [FilePath]
  , delegationCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  , scriptOut :: FilePath
  , scriptHashOut :: FilePath
  , scriptAddressOut :: FilePath
  , datumOutFile :: FilePath
  }

initColdNFTCommandParser :: ParserInfo InitColdNFTCommand
initColdNFTCommandParser = info parser description
  where
    description :: InfoMod InitColdNFTCommand
    description =
      progDesc "Initialize the cold NFT lock script by sending an NFT to it."

    parser :: Parser InitColdNFTCommand
    parser =
      InitColdNFTCommand
        <$> networkTypeParser
        <*> coldCredentialScriptFileParser
        <*> caCertFileParser
        <*> some membershipFileParser
        <*> some delegationFileParser
        <*> optional stakeCredentialFileParser
        <*> scriptOutParser
        <*> scriptHashOutParser
        <*> scriptAddressOutParser
        <*> datumOutParser

networkTypeParser :: Parser NetworkType
networkTypeParser =
  asum
    [ flag' Mainnet $
        fold
          [ long "mainnet"
          , help "Build a mainnet script address"
          ]
    , flag' Testnet $
        fold
          [ long "testnet"
          , help "Build a testnet script address"
          ]
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

coldCredentialScriptFileParser :: Parser FilePath
coldCredentialScriptFileParser =
  strOption $
    fold
      [ long "cold-credential-script-file"
      , metavar "FILE_PATH"
      , help "A relative path to the compiled cold credential script file."
      , action "file"
      ]

caCertFileParser :: Parser FilePath
caCertFileParser =
  strOption $
    fold
      [ long "ca-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the root CA certificate PEM file."
      , action "file"
      ]

membershipFileParser :: Parser FilePath
membershipFileParser =
  strOption $
    fold
      [ long "membership-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the certificate PEM file of a membership user."
      , action "file"
      ]

delegationFileParser :: Parser FilePath
delegationFileParser =
  strOption $
    fold
      [ long "delegation-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the certificate PEM file of a delegation user."
      , action "file"
      ]

scriptAddressOutParser :: Parser FilePath
scriptAddressOutParser =
  strOption $
    fold
      [ long "script-address-out-file"
      , metavar "FILE_PATH"
      , help
          "A relative path to the file where the script address should be written as a bech32 string."
      , action "file"
      ]

datumOutParser :: Parser FilePath
datumOutParser =
  strOption $
    fold
      [ long "datum-out-file"
      , metavar "FILE_PATH"
      , help
          "A relative path to the file where the initial datum should be written as JSON."
      , action "file"
      ]

writeBech32ToFile :: (SerialiseAsBech32 a) => FilePath -> a -> IO ()
writeBech32ToFile file = T.writeFile file . serialiseToBech32

runInitColdNFTCommand :: InitColdNFTCommand -> IO ()
runInitColdNFTCommand InitColdNFTCommand{..} = do
  coldCredentialScriptResult <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File coldCredentialScriptFile)

  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile

  coldCredentialScript <- case coldCredentialScriptResult of
    Left err -> do
      error $ "Failed to read cold credential script file: " <> show err
    Right script -> pure $ PlutusScript PlutusScriptV3 script

  certificateAuthority <- readIdentityFromPEMFile' caCertFile
  membershipUsers <- traverse readIdentityFromPEMFile' membershipCertFiles
  delegationUsers <- traverse readIdentityFromPEMFile' delegationCertFiles

  let coldCredentialScriptHash =
        ScriptHash $
          toBuiltin $
            serialiseToRawBytes $
              hashScript coldCredentialScript
  let coldCredential =
        ColdCommitteeCredential $ ScriptCredential coldCredentialScriptHash
  let compiledScript = Scripts.coldNFT coldCredential
  script <- writeScriptToFile scriptOut compiledScript

  let scriptHash = hashScript script
  writeHexBytesToFile scriptHashOut scriptHash

  let networkId = case networkType of
        Mainnet -> C.Mainnet
        -- the network magic is unimportant for building addresses
        Testnet -> C.Testnet $ C.NetworkMagic 1
  let paymentCredential = PaymentCredentialByScript scriptHash
  writeBech32ToFile scriptAddressOut $
    makeShelleyAddress networkId paymentCredential stakeAddress

  let datum = ColdLockDatum{..}
  let datumEncoded = unsafeHashableScriptData $ fromPlutusData $ toData datum
  LBS.writeFile datumOutFile $
    encodePretty $
      scriptDataToJsonDetailedSchema datumEncoded

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

readIdentityFromPEMFile' :: FilePath -> IO Identity
readIdentityFromPEMFile' file =
  readIdentityFromPEMFile file >>= \case
    Left err -> error $ file <> ": " <> err
    Right a -> pure a
