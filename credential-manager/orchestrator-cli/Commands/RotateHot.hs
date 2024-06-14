module Commands.RotateHot (
  RotateHotCommand (..),
  rotateHotCommandParser,
  runRotateHotCommand,
) where

import Commands.Common (
  outDirParser,
  readFileUTxO,
  readIdentityFromPEMFile',
  runCommand,
  utxoFileParser,
  votingCertParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Orchestrator.RotateHot
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
  some,
 )

data RotateHotCommand = RotateHotCommand
  { utxoFile :: FilePath
  , votingCerts :: [FilePath]
  , outDir :: FilePath
  }

rotateHotCommandParser :: ParserInfo RotateHotCommand
rotateHotCommandParser = info parser description
  where
    description :: InfoMod RotateHotCommand
    description =
      progDesc "Spend the hot NFT to rotate voting keys in the datum."

    parser :: Parser RotateHotCommand
    parser =
      RotateHotCommand
        <$> utxoFileParser
        <*> some votingCertParser
        <*> outDirParser

runRotateHotCommand :: RotateHotCommand -> IO ()
runRotateHotCommand RotateHotCommand{..} = do
  scriptUtxo <- readFileUTxO utxoFile
  newVoting <- traverse readIdentityFromPEMFile' votingCerts
  let inputs = RotateHotInputs{..}
  RotateHotOutputs{..} <- runCommand rotateHot inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
    EmptyVoting -> "No voting users specified"
    DuplicateVotingCertificates cert ->
      "Multiple voting users have the same certificate hash " <> show cert
    DuplicateVotingPubKeyHash key ->
      "Multiple voting users have the same public key hash " <> show key
    EmptyUTxO -> "Script UTxO is empty"
    AmbiguousUTxO -> "Script UTxO has more than one output"
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
