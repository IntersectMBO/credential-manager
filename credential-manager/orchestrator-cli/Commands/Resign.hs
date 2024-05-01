module Commands.Resign (
  ResignCommand (..),
  resignCommandParser,
  runResignCommand,
) where

import Cardano.Api.Ledger (
  AnchorData,
  SafeHash,
  StandardCrypto,
  Url,
 )
import Commands.Common (
  coldCredentialScriptFileParser,
  metadataHashParser,
  metadataUrlParser,
  outDirParser,
  readFilePlutusV3Script,
  readFileTxOut,
  runCommand,
  utxoFileParser,
  writeCertificateToFile,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Orchestrator.Resign
import Data.Foldable (Foldable (..))
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  help,
  info,
  progDesc,
 )

data ResignCommand = ResignCommand
  { utxoFile :: FilePath
  , coldCredentialScriptFile :: FilePath
  , metadataUrl :: Url
  , metadataHash :: SafeHash StandardCrypto AnchorData
  , outDir :: FilePath
  }

resignCommandParser :: ParserInfo ResignCommand
resignCommandParser = info parser description
  where
    description :: InfoMod ResignCommand
    description = progDesc "Spend the cold NFT to resign from the committee."

    parser :: Parser ResignCommand
    parser =
      ResignCommand
        <$> utxoFileParser
        <*> coldCredentialScriptFileParser
        <*> metadataUrlParser metadataInfo
        <*> metadataHashParser
        <*> outDirParser

metadataInfo :: Mod OptionFields Url
metadataInfo =
  fold
    [ help "URL of the governance metadata document giving context for resignation."
    ]

runResignCommand :: ResignCommand -> IO ()
runResignCommand ResignCommand{..} = do
  coldCredentialScript <- readFilePlutusV3Script coldCredentialScriptFile
  scriptUtxo <- readFileTxOut utxoFile
  let inputs = ResignInputs{..}
  ResignOutputs{..} <- runCommand resign inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
  writeCertificateToFile outDir "resign.cert" certificate
