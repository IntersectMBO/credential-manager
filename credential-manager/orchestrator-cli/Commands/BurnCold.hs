module Commands.BurnCold (
  BurnColdCommand (..),
  burnColdCommandParser,
  runBurnColdCommand,
) where

import Cardano.Api (PlutusScriptVersion (..), hashScript)
import Commands.Common (
  debugParser,
  extractTxIn,
  outDirParser,
  readFileUTxO,
  utxoFileParser,
  writeHexBytesToFile,
  writePlutusDataToFile,
  writeScriptToFile,
 )
import CredentialManager.Api (
  ColdLockRedeemer (..),
  MintingRedeemer (Burn),
 )
import qualified CredentialManager.Debug.ScriptsV2 as DebugV2
import CredentialManager.Orchestrator.Common (serialiseScript)
import qualified CredentialManager.ScriptsV2 as ScriptsV2
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data BurnColdCommand = BurnColdCommand
  { utxoFile :: FilePath
  , debug :: Bool
  , outDir :: FilePath
  }

burnColdCommandParser :: ParserInfo BurnColdCommand
burnColdCommandParser = info parser description
  where
    description :: InfoMod BurnColdCommand
    description = progDesc "Burn the cold NFT."

    parser :: Parser BurnColdCommand
    parser =
      BurnColdCommand
        <$> utxoFileParser
        <*> debugParser
        <*> outDirParser

runBurnColdCommand :: BurnColdCommand -> IO ()
runBurnColdCommand BurnColdCommand{..} = do
  utxo <- readFileUTxO utxoFile
  txIn <- extractTxIn utxo
  let mintingScript =
        serialiseScript
          PlutusScriptV2
          if debug
            then DebugV2.coldMinting
            else ScriptsV2.coldMinting
  let mintingScriptHash = hashScript mintingScript
  let mintingRedeemer = Burn txIn
  writeScriptToFile outDir "minting.plutus" mintingScript
  writeHexBytesToFile outDir "minting.plutus.hash" mintingScriptHash
  writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
  writePlutusDataToFile outDir "redeemer.json" BurnCold
