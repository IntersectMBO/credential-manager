module Commands (
  Command,
  commandParser,
  runCommand,
) where

import Commands.Authorize (
  AuthorizeCommand,
  authorizeCommandParser,
  runAuthorizeCommand,
 )
import Commands.BurnHot (
  BurnHotCommand,
  burnHotCommandParser,
  runBurnHotCommand,
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
import Commands.ResignDelegation (
  ResignDelegationCommand,
  resignDelegationCommandParser,
  runResignDelegationCommand,
 )
import Commands.ResignVoting (
  ResignVotingCommand,
  resignVotingCommandParser,
  runResignVotingCommand,
 )
import Commands.RotateCold (
  RotateColdCommand,
  rotateColdCommandParser,
  runRotateColdCommand,
 )
import Commands.RotateHot (
  RotateHotCommand,
  rotateHotCommandParser,
  runRotateHotCommand,
 )
import Commands.UnlockCold (
  UnlockColdCommand,
  runUnlockColdCommand,
  unlockColdCommandParser,
 )
import Commands.Vote (VoteCommand, runVoteCommand, voteCommandParser)
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
  | ResignDelegation ResignDelegationCommand
  | RotateCold RotateColdCommand
  | UnlockCold UnlockColdCommand

data HotNFTCommand
  = InitHotNFT InitHotNFTCommand
  | Vote VoteCommand
  | ResignVoting ResignVotingCommand
  | RotateHot RotateHotCommand
  | BurnHot BurnHotCommand

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
          , command "resign-delegation" $ ResignDelegation <$> resignDelegationCommandParser
          , command "rotate" $ RotateCold <$> rotateColdCommandParser
          , command "unlock" $ UnlockCold <$> unlockColdCommandParser
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
          , command "vote" $ Vote <$> voteCommandParser
          , command "resignVoting" $ ResignVoting <$> resignVotingCommandParser
          , command "rotate" $ RotateHot <$> rotateHotCommandParser
          , command "burn" $ BurnHot <$> burnHotCommandParser
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
  ResignDelegation cmd -> runResignDelegationCommand cmd
  RotateCold cmd -> runRotateColdCommand cmd
  UnlockCold cmd -> runUnlockColdCommand cmd

runHotNFTCommand :: HotNFTCommand -> IO ()
runHotNFTCommand = \case
  InitHotNFT cmd -> runInitHotNFTCommand cmd
  Vote cmd -> runVoteCommand cmd
  ResignVoting cmd -> runResignVotingCommand cmd
  RotateHot cmd -> runRotateHotCommand cmd
  BurnHot cmd -> runBurnHotCommand cmd
