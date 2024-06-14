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
import Options.Applicative (Parser, command, hsubparser)

data Command
  = InitCold InitColdCommand
  | Authorize AuthorizeCommand
  | Resign ResignCommand
  | ResignDelegation ResignDelegationCommand
  | RotateCold RotateColdCommand
  | UnlockCold UnlockColdCommand
  | InitHot InitHotCommand
  | Vote VoteCommand
  | ResignVoting ResignVotingCommand
  | RotateHot RotateHotCommand
  | UnlockHot UnlockHotCommand

-- Parsers

commandParser :: Parser Command
commandParser =
  hsubparser $
    fold
      [ command "init-cold" $ InitCold <$> initColdCommandParser
      , command "init-hot" $ InitHot <$> initHotCommandParser
      , command "authorize" $ Authorize <$> authorizeCommandParser
      , command "vote" $ Vote <$> voteCommandParser
      , command "resign-committee" $ Resign <$> resignCommandParser
      , command "resign-delegation" $ ResignDelegation <$> resignDelegationCommandParser
      , command "resign-voting" $ ResignVoting <$> resignVotingCommandParser
      , command "rotate-cold" $ RotateCold <$> rotateColdCommandParser
      , command "rotate-hot" $ RotateHot <$> rotateHotCommandParser
      , command "unlock-cold" $ UnlockCold <$> unlockColdCommandParser
      , command "unlock-hot" $ UnlockHot <$> unlockHotCommandParser
      ]

-- Implementations

runCommand :: Command -> IO ()
runCommand = \case
  InitCold cmd -> runInitColdCommand cmd
  Authorize cmd -> runAuthorizeCommand cmd
  Resign cmd -> runResignCommand cmd
  ResignDelegation cmd -> runResignDelegationCommand cmd
  RotateCold cmd -> runRotateColdCommand cmd
  UnlockCold cmd -> runUnlockColdCommand cmd
  InitHot cmd -> runInitHotCommand cmd
  Vote cmd -> runVoteCommand cmd
  ResignVoting cmd -> runResignVotingCommand cmd
  RotateHot cmd -> runRotateHotCommand cmd
  UnlockHot cmd -> runUnlockHotCommand cmd
