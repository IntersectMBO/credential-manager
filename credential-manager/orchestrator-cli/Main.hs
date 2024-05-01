module Main where

import Commands (runCommand)
import Options (Options (..), options)
import Options.Applicative (execParser)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = execParser options >>= run

run :: Options -> IO ()
run Options{..} = do
  hPutStrLn
    stderr
    "WARNING: This version of the credential management system has not been audited, and is not recommended for use on mainnet. "
  runCommand command
