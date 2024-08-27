-- We have to add Extra suffix so discovery process does not pick it up
module OrchestratorCLI.TestCommon.HSpecExtra where

import Test.Hspec ( aroundWith, SpecWith, Expectation, shouldSatisfy )
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, listDirectory)
import Turtle (directory)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import Test.HUnit (assertFailure, assertBool)
import Control.Monad (unless)
import Data.Foldable (for_)
import OrchestratorCLI.TestCommon.Turtle (catFileStrict)
import System.Environment (lookupEnv)

ignoreHookContext :: SpecWith () -> SpecWith a
ignoreHookContext = aroundWith (\actionRunner -> const (actionRunner ()))

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle = haystack `shouldSatisfy` T.isInfixOf needle

fileShouldExist :: FilePath -> Expectation
fileShouldExist path = do
    doesFileExist path >>= \exists -> do
      unless exists do
        files <- listDirectory (directory path)
        assertFailure ("File does not exist: " ++ path ++ "\nFiles in directory: " ++ show files)

type Stdout = Text
type Stderr = Text

type Script = IO (ExitCode, Stdout, Stderr)

shouldCreateFiles :: Foldable t => Script -> t FilePath -> IO ()
shouldCreateFiles script paths = do
    (exitCode, stdout, stderr) <- script
    do
      let errMsg = "Exit code was not successful: " ++ show exitCode ++ "\nStdout: " ++ T.unpack stdout ++ "\nStderr: " ++ T.unpack stderr
      assertBool errMsg (exitCode == ExitSuccess)

    for_ paths \path -> do
      doesFileExist path >>= \exists -> do
        unless exists do
          files <- listDirectory (directory path)
          assertFailure ("File was not created: " ++ path ++ "\nFiles in directory: " ++ show files)

shouldExecuteSuccessfully :: Script -> IO Text
shouldExecuteSuccessfully script = do
    (exitCode, stdout, stderr) <- script
    let errMsg = "Script failed. Exit code: " ++ show exitCode ++ "\nStdout: " ++ T.unpack stdout ++ "\nStderr: " ++ T.unpack stderr
    assertBool errMsg (exitCode == ExitSuccess)
    pure stdout


expectJust :: String -> Maybe a -> IO a
expectJust _ (Just a) = pure a
expectJust msg Nothing = assertFailure msg

catFile :: FilePath -> IO Text
catFile path = do
  (exitCode, content) <- catFileStrict path
  case exitCode of
    ExitSuccess -> pure content
    _ -> assertFailure $ "Failed to read file: " <> path

getEnvVar :: String -> IO String
getEnvVar var = do
  maybeValue <- lookupEnv var
  expectJust ("Expected " <> var <> " to be set") maybeValue

