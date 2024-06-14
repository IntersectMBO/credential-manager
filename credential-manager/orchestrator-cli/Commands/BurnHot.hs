module Commands.BurnHot (
  BurnHotCommand (..),
  burnHotCommandParser,
  runBurnHotCommand,
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
  HotLockRedeemer (..),
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

data BurnHotCommand = BurnHotCommand
  { utxoFile :: FilePath
  , debug :: Bool
  , outDir :: FilePath
  }

burnHotCommandParser :: ParserInfo BurnHotCommand
burnHotCommandParser = info parser description
  where
    description :: InfoMod BurnHotCommand
    description = progDesc "Burn the hot NFT."

    parser :: Parser BurnHotCommand
    parser =
      BurnHotCommand
        <$> utxoFileParser
        <*> debugParser
        <*> outDirParser

runBurnHotCommand :: BurnHotCommand -> IO ()
runBurnHotCommand BurnHotCommand{..} = do
  utxo <- readFileUTxO utxoFile
  txIn <- extractTxIn utxo
  let mintingScript =
        serialiseScript
          PlutusScriptV2
          if debug
            then DebugV2.hotMinting
            else ScriptsV2.hotMinting
  let mintingScriptHash = hashScript mintingScript
  let mintingRedeemer = Burn txIn
  writeScriptToFile outDir "minting.plutus" mintingScript
  writeHexBytesToFile outDir "minting.plutus.hash" mintingScriptHash
  writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
  writePlutusDataToFile outDir "redeemer.json" BurnHot
