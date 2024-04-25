module Commands where

import Commands.Authorize (
  AuthorizeCommand,
  authorizeCommandParser,
  runAuthorizeCommand,
 )
import Commands.InitColdCredential (
  InitColdCredentialCommand,
  initColdCredentialCommandParser,
  runInitColdCredentialCommand,
 )
import Commands.InitColdNFT (
  InitColdNFTCommand,
  initColdNFTCommandParser,
  runInitColdNFTCommand,
 )
import Commands.InitHotCredential (
  InitHotCredentialCommand,
  initHotCredentialCommandParser,
  runInitHotCredentialCommand,
 )
import Commands.InitHotNFT (
  InitHotNFTCommand,
  initHotNFTCommandParser,
  runInitHotNFTCommand,
 )
import Commands.Resign (ResignCommand, resignCommandParser, runResignCommand)
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
  = InitColdNFT InitColdNFTCommand
  | Authorize AuthorizeCommand
  | Resign ResignCommand

newtype HotNFTCommand
  = InitHotNFT InitHotNFTCommand

-- Parsers

commandParser :: Parser Command
commandParser =
  hsubparser $
    fold
      [ command "cold-credential" $ ColdCredential <$> coldCredentialCommandParser
      , command "hot-credential" $ HotCredential <$> hotCredentialCommandParser
      , command "cold-nft" $ ColdNFT <$> coldNFTCommandParser
      , command "hot-nft" $ HotNFT <$> hotNFTCommandParser
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

coldNFTCommandParser :: ParserInfo ColdNFTCommand
coldNFTCommandParser = info parser description
  where
    description :: InfoMod ColdNFTCommand
    description = progDesc "Manage the cold NFT locking script."

    parser :: Parser ColdNFTCommand
    parser =
      hsubparser $
        fold
          [ command "init" $ InitColdNFT <$> initColdNFTCommandParser
          , command "authorize" $ Authorize <$> authorizeCommandParser
          , command "resign" $ Resign <$> resignCommandParser
          ]

hotNFTCommandParser :: ParserInfo HotNFTCommand
hotNFTCommandParser = info parser description
  where
    description :: InfoMod HotNFTCommand
    description = progDesc "Manage the hot NFT locking script."

    parser :: Parser HotNFTCommand
    parser =
      hsubparser $
        fold
          [ command "init" $ InitHotNFT <$> initHotNFTCommandParser
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
runColdNFTCommand = \case
  InitColdNFT cmd -> runInitColdNFTCommand cmd
  Authorize cmd -> runAuthorizeCommand cmd
  Resign cmd -> runResignCommand cmd

runHotNFTCommand :: HotNFTCommand -> IO ()
runHotNFTCommand = \case
  InitHotNFT cmd -> runInitHotNFTCommand cmd
