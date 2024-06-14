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
import Commands.InitCold (
  InitColdCommand,
  initColdCommandParser,
  runInitColdCommand,
 )
import Commands.InitHot (
  InitHotCommand,
  initHotCommandParser,
  runInitHotCommand,
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
import Commands.UnlockHot (
  UnlockHotCommand,
  runUnlockHotCommand,
  unlockHotCommandParser,
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
  = Cold ColdCommand
  | Hot HotCommand

data ColdCommand
  = InitCold InitColdCommand
  | Authorize AuthorizeCommand
  | Resign ResignCommand
  | ResignDelegation ResignDelegationCommand
  | RotateCold RotateColdCommand
  | UnlockCold UnlockColdCommand

data HotCommand
  = InitHot InitHotCommand
  | Vote VoteCommand
  | ResignVoting ResignVotingCommand
  | RotateHot RotateHotCommand
  | UnlockHot UnlockHotCommand

-- Parsers

commandParser :: Parser Command
commandParser =
  hsubparser $
    fold
      [ command "cold" $ Cold <$> coldCommandParser
      , command "hot" $ Hot <$> hotCommandParser
      ]

coldCommandParser :: ParserInfo ColdCommand
coldCommandParser = info parser description
  where
    description :: InfoMod ColdCommand
    description = progDesc "Manage the cold scripts."

    parser :: Parser ColdCommand
    parser =
      hsubparser $
        fold
          [ command "init" $ InitCold <$> initColdCommandParser
          , command "authorize" $ Authorize <$> authorizeCommandParser
          , command "resign" $ Resign <$> resignCommandParser
          , command "resign-delegation" $ ResignDelegation <$> resignDelegationCommandParser
          , command "rotate" $ RotateCold <$> rotateColdCommandParser
          , command "unlock" $ UnlockCold <$> unlockColdCommandParser
          ]

hotCommandParser :: ParserInfo HotCommand
hotCommandParser = info parser description
  where
    description :: InfoMod HotCommand
    description = progDesc "Manage the hot scripts."

    parser :: Parser HotCommand
    parser =
      hsubparser $
        fold
          [ command "init" $ InitHot <$> initHotCommandParser
          , command "vote" $ Vote <$> voteCommandParser
          , command "resign-voting" $ ResignVoting <$> resignVotingCommandParser
          , command "rotate" $ RotateHot <$> rotateHotCommandParser
          , command "unlock" $ UnlockHot <$> unlockHotCommandParser
          ]

-- Implementations

runCommand :: Command -> IO ()
runCommand = \case
  Cold cmd -> runColdCommand cmd
  Hot cmd -> runHotCommand cmd

runColdCommand :: ColdCommand -> IO ()
runColdCommand = \case
  InitCold cmd -> runInitColdCommand cmd
  Authorize cmd -> runAuthorizeCommand cmd
  Resign cmd -> runResignCommand cmd
  ResignDelegation cmd -> runResignDelegationCommand cmd
  RotateCold cmd -> runRotateColdCommand cmd
  UnlockCold cmd -> runUnlockColdCommand cmd

runHotCommand :: HotCommand -> IO ()
runHotCommand = \case
  InitHot cmd -> runInitHotCommand cmd
  Vote cmd -> runVoteCommand cmd
  ResignVoting cmd -> runResignVotingCommand cmd
  RotateHot cmd -> runRotateHotCommand cmd
  UnlockHot cmd -> runUnlockHotCommand cmd
