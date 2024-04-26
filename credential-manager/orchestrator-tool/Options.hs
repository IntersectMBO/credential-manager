module Options (
  Options (..),
  options,
) where

import Commands (Command, commandParser)
import Data.Foldable (Foldable (..))
import Data.Version (showVersion)
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  progDesc,
  short,
 )
import Paths_credential_manager (version)

newtype Options = Options
  { command :: Command
  }

options :: ParserInfo Options
options = info parser description

parser :: Parser Options
parser = helper <*> versionOption <*> optionsParser

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("orchestrator-tool " <> showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

description :: InfoMod Options
description =
  fold
    [ fullDesc
    , progDesc "Tool for building components of credential management transactions."
    , header
        "orchestrator-tool: a tool for building credential management transaction."
    ]
