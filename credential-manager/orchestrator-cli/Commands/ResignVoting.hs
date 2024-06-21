module Commands.ResignVoting (
  ResignVotingCommand (..),
  resignVotingCommandParser,
  runResignVotingCommand,
) where

import Commands.Common (
  checkGroupSize,
  outDirParser,
  readFileUTxO,
  readIdentityFromPEMFile',
  runCommand,
  utxoFileParser,
  votingCertParser,
  writePlutusDataToFile,
  writeTxOutValueToFile,
 )
import CredentialManager.Api (HotLockDatum (..))
import CredentialManager.Orchestrator.ResignVoting
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data ResignVotingCommand = ResignVotingCommand
  { utxoFile :: FilePath
  , resigneeCert :: FilePath
  , outDir :: FilePath
  }

resignVotingCommandParser :: ParserInfo ResignVotingCommand
resignVotingCommandParser = info parser description
  where
    description :: InfoMod ResignVotingCommand
    description =
      progDesc "Spend the hot NFT to remove a voting user from the datum."

    parser :: Parser ResignVotingCommand
    parser =
      ResignVotingCommand
        <$> utxoFileParser
        <*> votingCertParser
        <*> outDirParser

runResignVotingCommand :: ResignVotingCommand -> IO ()
runResignVotingCommand ResignVotingCommand{..} = do
  scriptUtxo <- readFileUTxO utxoFile
  resignee <- readIdentityFromPEMFile' resigneeCert
  let inputs = ResignVotingInputs{..}
  ResignVotingOutputs{..} <- runCommand resignVoting inputs \case
    AddressIsByron -> "UTxO has a Byron address."
    AddressIsPayment -> "UTxO has a payment address, script address expected."
    MissingDatum -> "UTxO has no datum present."
    NonInlineDatum -> "UTxO has a non-inline datum present."
    InvalidDatum -> "UTxO has an invalid datum."
    ResigneeNotInVotingGroup -> "Resignee is not in the voting group."
    EmptyVoting -> "Resignee is the last member of the voting group."
    EmptyUTxO -> "Script UTxO is empty"
    AmbiguousUTxO -> "Script UTxO has more than one output"
  let HotLockDatum{..} = outputDatum
  checkGroupSize "voting" votingUsers
  writePlutusDataToFile outDir "redeemer.json" redeemer
  writePlutusDataToFile outDir "datum.json" outputDatum
  writeTxOutValueToFile outDir "value" outputAddress outputValue
