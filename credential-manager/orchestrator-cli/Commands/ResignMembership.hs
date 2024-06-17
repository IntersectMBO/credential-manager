module Commands.ResignMembership (
  ResignMembershipCommand (..),
  resignMembershipCommandParser,
  runResignMembershipCommand,
) where

import Commands.Common (
  checkGroupSize,
  membershipCertParser,
  outDirParser,
  readFileUTxO,
  readIdentityFromPEMFile',
  runCommand,
  utxoFileParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Api (ColdLockDatum (..))
import CredentialManager.Orchestrator.ResignMembership
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data ResignMembershipCommand = ResignMembershipCommand
  { utxoFile :: FilePath
  , resigneeCert :: FilePath
  , outDir :: FilePath
  }

resignMembershipCommandParser :: ParserInfo ResignMembershipCommand
resignMembershipCommandParser = info parser description
  where
    description :: InfoMod ResignMembershipCommand
    description =
      progDesc "Spend the cold NFT to remove a membership user from the datum."

    parser :: Parser ResignMembershipCommand
    parser =
      ResignMembershipCommand
        <$> utxoFileParser
        <*> membershipCertParser
        <*> outDirParser

runResignMembershipCommand :: ResignMembershipCommand -> IO ()
runResignMembershipCommand ResignMembershipCommand{..} = do
  scriptUtxo <- readFileUTxO utxoFile
  resignee <- readIdentityFromPEMFile' resigneeCert
  let inputs = ResignMembershipInputs{..}
  ResignMembershipOutputs{..} <- runCommand resignMembership inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
    ResigneeNotInMembershipGroup -> "Resignee is not in the membership group."
    EmptyMembership -> "Resignee is the last member of the membership group."
    EmptyUTxO -> "Script UTxO is empty"
    AmbiguousUTxO -> "Script UTxO has more than one output"
  let ColdLockDatum{..} = outputDatum
  checkGroupSize "membership" membershipUsers
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
