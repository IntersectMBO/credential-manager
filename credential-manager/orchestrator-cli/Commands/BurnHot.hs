module Commands.BurnHot (
  BurnHotCommand (..),
  burnHotCommandParser,
  runBurnHotCommand,
) where

import Commands.Common (
  extractTxIn,
  outDirParser,
  readFileUTxO,
  utxoFileParser,
  writePlutusDataToFile,
 )
import CredentialManager.Api (
  HotLockRedeemer (..),
  MintingRedeemer (Burn),
 )
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

data BurnHotCommand = BurnHotCommand
  { utxoFile :: FilePath
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
        <*> outDirParser

runBurnHotCommand :: BurnHotCommand -> IO ()
runBurnHotCommand BurnHotCommand{..} = do
  utxo <- readFileUTxO utxoFile
  txIn <- extractTxIn utxo
  let mintingRedeemer = Burn txIn
  writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
  writePlutusDataToFile outDir "nft.redeemer.json" BurnHot
