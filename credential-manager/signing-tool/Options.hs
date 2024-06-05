module Options where

import Data.Foldable (Foldable (..))
import Data.Version (showVersion)
import Options.Applicative
import Paths_credential_manager (version)

newtype Options = Options
  { configFile :: Maybe FilePath
  }

options :: ParserInfo Options
options = info parser description

parser :: Parser Options
parser = helper <*> versionInfo <*> optionsParser

optionsParser :: Parser Options
optionsParser = Options <$> configFileParser

versionInfo :: Parser (Options -> Options)
versionInfo =
  infoOption
    ("signing-tool " <> showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

description :: InfoMod Options
description =
  fold
    [ fullDesc
    , progDesc "GTK GUI application for signing CC Credential Manager transactions."
    ]

configFileParser :: Parser (Maybe FilePath)
configFileParser =
  optional $
    strOption $
      fold
        [ long "config-file"
        , short 'c'
        , metavar "FILEPATH"
        , help "The JSON config file"
        , action "file"
        ]
