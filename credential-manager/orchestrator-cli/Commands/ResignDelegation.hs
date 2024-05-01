module Commands.ResignDelegation (
  ResignDelegationCommand (..),
  resignDelegationCommandParser,
  runResignDelegationCommand,
) where

import Commands.Common (
  delegationCertParser,
  outDirParser,
  readFileTxOut,
  readIdentityFromPEMFile',
  runCommand,
  utxoFileParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Orchestrator.ResignDelegation
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data ResignDelegationCommand = ResignDelegationCommand
  { utxoFile :: FilePath
  , resigneeCert :: FilePath
  , outDir :: FilePath
  }

resignDelegationCommandParser :: ParserInfo ResignDelegationCommand
resignDelegationCommandParser = info parser description
  where
    description :: InfoMod ResignDelegationCommand
    description =
      progDesc "Spend the cold NFT to remove a delegation user from the datum."

    parser :: Parser ResignDelegationCommand
    parser =
      ResignDelegationCommand
        <$> utxoFileParser
        <*> delegationCertParser
        <*> outDirParser

runResignDelegationCommand :: ResignDelegationCommand -> IO ()
runResignDelegationCommand ResignDelegationCommand{..} = do
  scriptUtxo <- readFileTxOut utxoFile
  resignee <- readIdentityFromPEMFile' resigneeCert
  let inputs = ResignDelegationInputs{..}
  ResignDelegationOutputs{..} <- runCommand resignDelegation inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
    ResigneeNotInDelegationGroup -> "Resignee is not in the delegation group."
    EmptyDelegation -> "Resignee is the last member of the delegation group."
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
