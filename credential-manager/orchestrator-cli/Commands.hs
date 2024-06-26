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
import Commands.BurnCold (
  BurnColdCommand,
  burnColdCommandParser,
  runBurnColdCommand,
 )
import Commands.BurnHot (
  BurnHotCommand,
  burnHotCommandParser,
  runBurnHotCommand,
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
import Commands.ResignMembership (
  ResignMembershipCommand,
  resignMembershipCommandParser,
  runResignMembershipCommand,
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
import Commands.UpgradeCold (
  UpgradeColdCommand,
  runUpgradeColdCommand,
  upgradeColdCommandParser,
 )
import Commands.UpgradeHot (
  UpgradeHotCommand,
  runUpgradeHotCommand,
  upgradeHotCommandParser,
 )
import Commands.Vote (VoteCommand, runVoteCommand, voteCommandParser)
import Data.Foldable (Foldable (..))
import Options.Applicative (Parser, command, hsubparser)

data Command
  = InitCold InitColdCommand
  | Authorize AuthorizeCommand
  | Resign ResignCommand
  | ResignMembership ResignMembershipCommand
  | ResignDelegation ResignDelegationCommand
  | RotateCold RotateColdCommand
  | BurnCold BurnColdCommand
  | UpgradeCold UpgradeColdCommand
  | InitHot InitHotCommand
  | Vote VoteCommand
  | ResignVoting ResignVotingCommand
  | RotateHot RotateHotCommand
  | BurnHot BurnHotCommand
  | UpgradeHot UpgradeHotCommand

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
      , command "resign-membership" $ ResignMembership <$> resignMembershipCommandParser
      , command "resign-delegation" $ ResignDelegation <$> resignDelegationCommandParser
      , command "resign-voting" $ ResignVoting <$> resignVotingCommandParser
      , command "rotate-cold" $ RotateCold <$> rotateColdCommandParser
      , command "rotate-hot" $ RotateHot <$> rotateHotCommandParser
      , command "burn-cold" $ BurnCold <$> burnColdCommandParser
      , command "burn-hot" $ BurnHot <$> burnHotCommandParser
      , command "upgrade-cold" $ UpgradeCold <$> upgradeColdCommandParser
      , command "upgrade-hot" $ UpgradeHot <$> upgradeHotCommandParser
      ]

-- Implementations

runCommand :: Command -> IO ()
runCommand = \case
  InitCold cmd -> runInitColdCommand cmd
  Authorize cmd -> runAuthorizeCommand cmd
  Resign cmd -> runResignCommand cmd
  ResignMembership cmd -> runResignMembershipCommand cmd
  ResignDelegation cmd -> runResignDelegationCommand cmd
  RotateCold cmd -> runRotateColdCommand cmd
  BurnCold cmd -> runBurnColdCommand cmd
  InitHot cmd -> runInitHotCommand cmd
  Vote cmd -> runVoteCommand cmd
  ResignVoting cmd -> runResignVotingCommand cmd
  RotateHot cmd -> runRotateHotCommand cmd
  BurnHot cmd -> runBurnHotCommand cmd
  UpgradeHot cmd -> runUpgradeHotCommand cmd
  UpgradeCold cmd -> runUpgradeColdCommand cmd
