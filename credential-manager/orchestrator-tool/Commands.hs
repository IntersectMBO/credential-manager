module Commands where

import Commands.InitColdCredential (
  InitColdCredentialCommand,
  initColdCredentialCommandParser,
  runInitColdCredentialCommand,
 )
import Commands.InitHotCredential (
  InitHotCredentialCommand,
  initHotCredentialCommandParser,
  runInitHotCredentialCommand,
 )
import Data.Foldable (Foldable (..))
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  command,
  hsubparser,
  info,
  progDesc,
 )

data Command
  = ColdCredential ColdCredentialCommand
  | HotCredential HotCredentialCommand
  | ColdNFT ColdNFTCommand
  | HotNFT HotNFTCommand

newtype ColdCredentialCommand = InitColdCredential InitColdCredentialCommand

newtype HotCredentialCommand = InitHotCredential InitHotCredentialCommand

data ColdNFTCommand

data HotNFTCommand

-- Parsers

commandParser :: Parser Command
commandParser =
  hsubparser $
    fold
      [ command "cold-credential" $ ColdCredential <$> coldCredentialCommandParser
      , command "hot-credential" $ HotCredential <$> hotCredentialCommandParser
      ]

coldCredentialCommandParser :: ParserInfo ColdCredentialCommand
coldCredentialCommandParser = info parser description
  where
    description :: InfoMod ColdCredentialCommand
    description = progDesc "Manage the cold CC credential script."

    parser :: Parser ColdCredentialCommand
    parser =
      hsubparser $
        fold
          [ command "init" $ InitColdCredential <$> initColdCredentialCommandParser
          ]

hotCredentialCommandParser :: ParserInfo HotCredentialCommand
hotCredentialCommandParser = info parser description
  where
    description :: InfoMod HotCredentialCommand
    description = progDesc "Manage the hot CC credential script."

    parser :: Parser HotCredentialCommand
    parser =
      hsubparser $
        fold
          [ command "init" $ InitHotCredential <$> initHotCredentialCommandParser
          ]

-- Implementations

runCommand :: Command -> IO ()
runCommand = \case
  ColdCredential cmd -> runColdCredentialCommand cmd
  HotCredential cmd -> runHotCredentialCommand cmd
  ColdNFT cmd -> runColdNFTCommand cmd
  HotNFT cmd -> runHotNFTCommand cmd

runColdCredentialCommand :: ColdCredentialCommand -> IO ()
runColdCredentialCommand = \case
  InitColdCredential cmd -> runInitColdCredentialCommand cmd

runHotCredentialCommand :: HotCredentialCommand -> IO ()
runHotCredentialCommand = \case
  InitHotCredential cmd -> runInitHotCredentialCommand cmd

runColdNFTCommand :: ColdNFTCommand -> IO ()
runColdNFTCommand = \case {}

runHotNFTCommand :: HotNFTCommand -> IO ()
runHotNFTCommand = \case {}
