module Commands.RotateCold (
  RotateColdCommand (..),
  rotateColdCommandParser,
  runRotateColdCommand,
) where

import Commands.Common (
  delegationCertParser,
  membershipCertParser,
  outDirParser,
  readFileTxOut,
  readIdentityFromPEMFile',
  runCommand,
  utxoFileParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Orchestrator.RotateCold
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
  some,
 )

data RotateColdCommand = RotateColdCommand
  { utxoFile :: FilePath
  , membershipCerts :: [FilePath]
  , delegationCerts :: [FilePath]
  , outDir :: FilePath
  }

rotateColdCommandParser :: ParserInfo RotateColdCommand
rotateColdCommandParser = info parser description
  where
    description :: InfoMod RotateColdCommand
    description =
      progDesc
        "Spend the cold NFT to rotate membership and delegation keys in the datum."

    parser :: Parser RotateColdCommand
    parser =
      RotateColdCommand
        <$> utxoFileParser
        <*> some membershipCertParser
        <*> some delegationCertParser
        <*> outDirParser

runRotateColdCommand :: RotateColdCommand -> IO ()
runRotateColdCommand RotateColdCommand{..} = do
  scriptUtxo <- readFileTxOut utxoFile
  newMembership <- traverse readIdentityFromPEMFile' membershipCerts
  newDelegation <- traverse readIdentityFromPEMFile' delegationCerts
  let inputs = RotateColdInputs{..}
  RotateColdOutputs{..} <- runCommand rotateCold inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
    EmptyMembership -> "No membership users specified"
    DuplicateMembershipCertificates cert ->
      "Multiple membership users have the same certificate hash " <> show cert
    DuplicateMembershipPubKeyHash key ->
      "Multiple membership users have the same public key hash " <> show key
    EmptyDelegation -> "No delegation users specified"
    DuplicateDelegationCertificates cert ->
      "Multiple delegation users have the same certificate hash " <> show cert
    DuplicateDelegationPubKeyHash key ->
      "Multiple delegation users have the same public key hash " <> show key
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
