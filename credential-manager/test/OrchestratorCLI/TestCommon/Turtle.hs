module OrchestratorCLI.TestCommon.Turtle where

import Control.Concurrent.Async (Async)
import Turtle hiding (cat)
import Data.Text (unpack)
import Control.Exception (throwIO)
import qualified Control.Concurrent.Async as Async
import qualified System.Process as Process
import qualified Data.Text.IO as Text
import Control.Exception.Base (try)
import qualified Data.Text as T

procGroup
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Line
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
procGroup cmd args line = do
    system
        ( (Process.proc (unpack cmd) (map unpack args))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            , Process.create_group = True
            } )
        line

inprocGroupWithErr
    :: Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Line
    -- ^ Lines of standard input
    -> Shell (Either Line Line)
    -- ^ Lines of either standard output (`Right`) or standard error (`Left`)
inprocGroupWithErr cmd args = do
  let
    pg = (Process.proc (unpack cmd) (map unpack args)) { Process.create_group = True }
  streamWithErr pg

outputWithErr
  :: MonadIO io
  => FilePath
  -- ^ Standard output file
  -> FilePath
  -- ^ Standard error file
  -> Shell (Either Line Line)
  -- ^ Lines of either standard output (`Right`) or standard error (`Left`)
  -> io ()
outputWithErr stdoutFile stderrFile process = sh (do
  stdoutHandle <- using (writeonly stdoutFile)
  stderrHandle <- using (writeonly stderrFile)
  process >>= \case
      Left line -> liftIO (Text.hPutStrLn stderrHandle (lineToText line))
      Right line -> liftIO (Text.hPutStrLn stdoutHandle (lineToText line))
  )

procGroupToFiles
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Line
    -- ^ Lines of standard input
    -> FilePath
    -- ^ Standard output file
    -> FilePath
    -- ^ Standard error file
    -> io ExitCode
    -- ^ Exit code
procGroupToFiles cmd args stdinPipe stdoutFile stderrFile= do
  liftIO $ try (outputWithErr stdoutFile stderrFile (inprocGroupWithErr cmd args stdinPipe)) >>= either pure (const $ pure ExitSuccess)

halt :: Async a -> IO ()
halt a = do
  m <- Async.poll a
  case m of
      Nothing -> Async.cancel a
      Just (Left  ex) -> throwIO ex
      Just (Right _) -> pure ()

catFileStrict :: MonadIO io => FilePath -> io (ExitCode, Text)
catFileStrict file = do
  procStrict "cat" [T.pack file] empty
