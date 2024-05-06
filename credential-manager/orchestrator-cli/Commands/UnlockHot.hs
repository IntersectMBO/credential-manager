module Commands.UnlockHot (
  UnlockHotCommand (..),
  unlockHotCommandParser,
  runUnlockHotCommand,
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

newtype UnlockHotCommand = UnlockHotCommand
  { outDir :: FilePath
  }

unlockHotCommandParser :: ParserInfo UnlockHotCommand
unlockHotCommandParser = info parser description
  where
    description :: InfoMod UnlockHotCommand
    description = progDesc "Unlock the hot NFT."

    parser :: Parser UnlockHotCommand
    parser = UnlockHotCommand <$> outDirParser

runUnlockHotCommand :: UnlockHotCommand -> IO ()
runUnlockHotCommand UnlockHotCommand{..} = writePlutusDataToFile outDir "redeemer.json" UnlockHot
