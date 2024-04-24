module Commands.InitColdCredential where

import Cardano.Api (
  AsType (AsPolicyId),
  File (..),
  PlutusScriptV3,
  PlutusScriptVersion (PlutusScriptV3),
  Script (PlutusScript),
  SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes),
  SerialiseAsRawBytesError (unSerialiseAsRawBytesError),
  hashScript,
  serialiseToRawBytesHexText,
  writeFileTextEnvelope,
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import qualified CredentialManager.Scripts as Scripts
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16Untyped)
import Data.Foldable (Foldable (..))
import Data.String (IsString (..))
import qualified Data.Text.IO as T
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  ReadM,
  action,
  eitherReader,
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
  readerError,
  short,
  strOption,
 )
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), toBuiltin)
import qualified PlutusLedgerApi.V3 as PlutusV1
import PlutusTx (CompiledCode)

data InitColdCredentialCommand = InitColdCredentialCommand
  { policyId :: CurrencySymbol
  , scriptOut :: FilePath
  , scriptHashOut :: FilePath
  }

initColdCredentialCommandParser :: ParserInfo InitColdCredentialCommand
initColdCredentialCommandParser = info parser description
  where
    description :: InfoMod InitColdCredentialCommand
    description =
      progDesc
        "Initialize the cold CC credential script by associating an NFT with it. The script will check that the NFT is spent whenever it needs to witness a transaction."

    parser :: Parser InitColdCredentialCommand
    parser =
      InitColdCredentialCommand
        <$> policyIdParser policyIdInfo
        <*> scriptOutParser
        <*> scriptHashOutParser

scriptOutParser :: Parser FilePath
scriptOutParser =
  strOption $
    fold
      [ long "script-out-file"
      , metavar "FILE_PATH"
      , help
          "A relative path to the file where the compiled script should be written as a text envelope."
      , action "file"
      ]

scriptHashOutParser :: Parser FilePath
scriptHashOutParser =
  strOption $
    fold
      [ long "script-hash-out-file"
      , metavar "FILE_PATH"
      , help
          "A relative path to the file where the compiled script should be written as a hexadecimal string."
      , action "file"
      ]

policyIdInfo :: Mod OptionFields CurrencySymbol
policyIdInfo =
  fold
    [ long "policy-id"
    , short 'p'
    , help "The minting policy ID of the NFT to associate with the cold credential"
    ]

policyIdParser :: Mod OptionFields CurrencySymbol -> Parser CurrencySymbol
policyIdParser = option readCurrencySymbol . (<> metavar "POLICY_ID")

readCurrencySymbol :: ReadM CurrencySymbol
readCurrencySymbol = do
  bytes <- readBase16
  policyId <-
    either
      (readerError . unSerialiseAsRawBytesError)
      pure
      $ deserialiseFromRawBytes AsPolicyId bytes
  pure $ CurrencySymbol $ toBuiltin $ serialiseToRawBytes policyId

readBase16 :: ReadM ByteString
readBase16 =
  eitherReader $
    first (const "Invalid hexadecimal text") . decodeBase16Untyped . fromString

runInitColdCredentialCommand :: InitColdCredentialCommand -> IO ()
runInitColdCredentialCommand InitColdCredentialCommand{..} = do
  let compiledScript = Scripts.coldCommittee policyId
  script <- writeScriptToFile scriptOut compiledScript
  writeHexBytesToFile scriptHashOut $ hashScript script

writeHexBytesToFile :: (SerialiseAsRawBytes a) => FilePath -> a -> IO ()
writeHexBytesToFile file = T.writeFile file . serialiseToRawBytesHexText

writeScriptToFile :: FilePath -> CompiledCode a -> IO (Script PlutusScriptV3)
writeScriptToFile file code = do
  either (error . show) pure
    =<< writeFileTextEnvelope (File file) Nothing plutusScript
  pure $ PlutusScript PlutusScriptV3 plutusScript
  where
    plutusScript = PlutusScriptSerialised $ PlutusV1.serialiseCompiledCode code
