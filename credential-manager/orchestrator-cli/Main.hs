module Main where

import Commands (runCommand)
import Options (Options (..), options)
import Options.Applicative (execParser)

main :: IO ()
main = execParser options >>= run

run :: Options -> IO ()
run Options{..} = runCommand command
