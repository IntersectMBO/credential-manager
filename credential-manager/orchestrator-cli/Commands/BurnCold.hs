module Commands.BurnCold (
  BurnColdCommand (..),
  burnColdCommandParser,
  runBurnColdCommand,
) where

import Commands.Common (
  extractTxIn,
  outDirParser,
  readFileUTxO,
  utxoFileParser,
  writePlutusDataToFile,
 )
import CredentialManager.Api (
  ColdLockRedeemer (..),
  MintingRedeemer (Burn),
 )
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data BurnColdCommand = BurnColdCommand
  { utxoFile :: FilePath
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
        <*> outDirParser

runBurnColdCommand :: BurnColdCommand -> IO ()
runBurnColdCommand BurnColdCommand{..} = do
  utxo <- readFileUTxO utxoFile
  txIn <- extractTxIn utxo
  let mintingRedeemer = Burn txIn
  writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
  writePlutusDataToFile outDir "nft.redeemer.json" BurnCold
