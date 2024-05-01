module Commands.Authorize (
  AuthorizeCommand (..),
  authorizeCommandParser,
  runAuthorizeCommand,
) where

import Commands.Common (
  coldCredentialScriptFileParser,
  hotCredentialScriptFileParser,
  outDirParser,
  readFilePlutusV3Script,
  readFileTxOut,
  runCommand,
  utxoFileParser,
  writeCertificateToFile,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Orchestrator.AuthorizeHot
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data AuthorizeCommand = AuthorizeCommand
  { utxoFile :: FilePath
  , coldCredentialScriptFile :: FilePath
  , hotCredentialScriptFile :: FilePath
  , outDir :: FilePath
  }

authorizeCommandParser :: ParserInfo AuthorizeCommand
authorizeCommandParser = info parser description
  where
    description :: InfoMod AuthorizeCommand
    description =
      progDesc "Spend the cold NFT to authorize a hot credential."

    parser :: Parser AuthorizeCommand
    parser =
      AuthorizeCommand
        <$> utxoFileParser
        <*> coldCredentialScriptFileParser
        <*> hotCredentialScriptFileParser
        <*> outDirParser

runAuthorizeCommand :: AuthorizeCommand -> IO ()
runAuthorizeCommand AuthorizeCommand{..} = do
  coldCredentialScript <- readFilePlutusV3Script coldCredentialScriptFile
  hotCredentialScript <- readFilePlutusV3Script hotCredentialScriptFile
  scriptUtxo <- readFileTxOut utxoFile
  let inputs = AuthorizeHotInputs{..}
  AuthorizeHotOutputs{..} <- runCommand authorizeHot inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
  writeCertificateToFile outDir "authorizeHot.cert" certificate
