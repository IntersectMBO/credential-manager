module Commands.UnlockCold (
  UnlockColdCommand (..),
  unlockColdCommandParser,
  runUnlockColdCommand,
) where

import Commands.Common (
  outDirParser,
  writePlutusDataToFile,
 )
import CredentialManager.Api (
  ColdLockRedeemer (..),
 )
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )

newtype UnlockColdCommand = UnlockColdCommand
  { outDir :: FilePath
  }

unlockColdCommandParser :: ParserInfo UnlockColdCommand
unlockColdCommandParser = info parser description
  where
    description :: InfoMod UnlockColdCommand
    description = progDesc "Unlock the cold NFT."

    parser :: Parser UnlockColdCommand
    parser = UnlockColdCommand <$> outDirParser

runUnlockColdCommand :: UnlockColdCommand -> IO ()
runUnlockColdCommand UnlockColdCommand{..} = writePlutusDataToFile outDir "redeemer.json" Unlock
