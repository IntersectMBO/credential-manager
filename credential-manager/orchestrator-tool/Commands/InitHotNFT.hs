module Commands.InitHotNFT where

import Cardano.Api (
  AsType (..),
  File (..),
  PaymentCredential (..),
  PlutusScriptVersion (..),
  PolicyId,
  Script (..),
  SerialiseAsRawBytes (..),
  StakeAddressReference (..),
  hashScript,
  makeShelleyAddress,
  readFileTextEnvelope,
  unsafeHashableScriptData,
 )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (fromPlutusData, scriptDataToJsonDetailedSchema)
import Commands.InitColdCredential (
  policyIdParser,
  scriptHashOutParser,
  scriptOutParser,
  writeHexBytesToFile,
  writeScriptToFile,
 )
import Commands.InitColdNFT (
  NetworkType (..),
  StakeCredentialFile,
  datumOutParser,
  networkTypeParser,
  readIdentityFromPEMFile',
  readStakeAddressFile,
  scriptAddressOutParser,
  stakeCredentialFileParser,
  writeBech32ToFile,
 )
import CredentialManager.Api (
  HotLockDatum (..),
 )
import qualified CredentialManager.Scripts as Scripts
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (Foldable (..))
import Options.Applicative (
  Alternative (some),
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  action,
  help,
  info,
  long,
  metavar,
  optional,
  progDesc,
  strOption,
 )
import PlutusLedgerApi.V3 (
  Credential (..),
  CurrencySymbol (..),
  HotCommitteeCredential (..),
  ScriptHash (..),
  toBuiltin,
  toData,
 )

data InitHotNFTCommand = InitHotNFTCommand
  { networkType :: NetworkType
  , coldNFTPolicyId :: PolicyId
  , hotNFTPolicyId :: PolicyId
  , hotCredentialScriptFile :: FilePath
  , votingCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  , scriptOut :: FilePath
  , scriptHashOut :: FilePath
  , scriptAddressOut :: FilePath
  , datumOutFile :: FilePath
  }

initHotNFTCommandParser :: ParserInfo InitHotNFTCommand
initHotNFTCommandParser = info parser description
  where
    description :: InfoMod InitHotNFTCommand
    description =
      progDesc "Initialize the hot NFT lock script by sending an NFT to it."

    parser :: Parser InitHotNFTCommand
    parser =
      InitHotNFTCommand
        <$> networkTypeParser
        <*> policyIdParser coldNFTPolicyIdInfo
        <*> policyIdParser hotNFTPolicyIdInfo
        <*> hotCredentialScriptFileParser
        <*> some votingCertFileParser
        <*> optional stakeCredentialFileParser
        <*> scriptOutParser
        <*> scriptHashOutParser
        <*> scriptAddressOutParser
        <*> datumOutParser

hotCredentialScriptFileParser :: Parser FilePath
hotCredentialScriptFileParser =
  strOption $
    fold
      [ long "hot-credential-script-file"
      , metavar "FILE_PATH"
      , help "A relative path to the compiled hot credential script file."
      , action "file"
      ]

votingCertFileParser :: Parser FilePath
votingCertFileParser =
  strOption $
    fold
      [ long "voting-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the certificate PEM file of a voting user."
      , action "file"
      ]

coldNFTPolicyIdInfo :: Mod OptionFields PolicyId
coldNFTPolicyIdInfo =
  fold
    [ long "cold-nft-policy-id"
    , help "The minting policy ID of the cold NFT."
    ]

hotNFTPolicyIdInfo :: Mod OptionFields PolicyId
hotNFTPolicyIdInfo =
  fold
    [ long "hot-nft-policy-id"
    , help "The minting policy ID of the hot NFT."
    ]

runInitHotNFTCommand :: InitHotNFTCommand -> IO ()
runInitHotNFTCommand InitHotNFTCommand{..} = do
  hotCredentialScriptResult <-
    readFileTextEnvelope
      (AsPlutusScript AsPlutusScriptV3)
      (File hotCredentialScriptFile)

  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile

  hotCredentialScript <- case hotCredentialScriptResult of
    Left err -> do
      error $ "Failed to read hot credential script file: " <> show err
    Right script -> pure $ PlutusScript PlutusScriptV3 script

  votingUsers <- traverse readIdentityFromPEMFile' votingCertFiles

  let hotCredentialScriptHash =
        ScriptHash $
          toBuiltin $
            serialiseToRawBytes $
              hashScript hotCredentialScript
  let hotCredential =
        HotCommitteeCredential $ ScriptCredential hotCredentialScriptHash
  let coldCurrency =
        CurrencySymbol $ toBuiltin $ serialiseToRawBytes coldNFTPolicyId
  let hotCurrency =
        CurrencySymbol $ toBuiltin $ serialiseToRawBytes hotNFTPolicyId
  let compiledScript = Scripts.hotNFT coldCurrency hotCurrency hotCredential
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

  let datum = HotLockDatum{..}
  let datumEncoded = unsafeHashableScriptData $ fromPlutusData $ toData datum
  LBS.writeFile datumOutFile $
    encodePretty $
      scriptDataToJsonDetailedSchema datumEncoded
