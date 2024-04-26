module Commands.BurnHot (
  BurnHotCommand (..),
  burnHotCommandParser,
  runBurnHotCommand,
) where

import Commands.Common (
  outDirParser,
  writePlutusDataToFile,
 )
import CredentialManager.Api (
  HotLockRedeemer (..),
 )
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

newtype BurnHotCommand = BurnHotCommand
  { outDir :: FilePath
  }

burnHotCommandParser :: ParserInfo BurnHotCommand
burnHotCommandParser = info parser description
  where
    description :: InfoMod BurnHotCommand
    description = progDesc "Burn the hot NFT."

    parser :: Parser BurnHotCommand
    parser = BurnHotCommand <$> outDirParser

runBurnHotCommand :: BurnHotCommand -> IO ()
runBurnHotCommand BurnHotCommand{..} = writePlutusDataToFile outDir "redeemer.json" BurnHot
