{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Exception (catch)
import Data.Functor (void)
import Data.GI.Base
import Data.Int (Int32)
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (ExitFailure))
import qualified GI.Gtk as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (signalE0)
import System.Exit (exitSuccess, exitWith)

main :: IO ()
main = runApp `catch` \(e :: GError) -> gerrorMessage e >>= putStrLn . T.unpack

runApp :: IO ()
runApp = do
  app <- new G.Application [#applicationId := "iog.signing-tool"]
  appNetwork <- compile do
    activateE <- signalE0 app #activate
    let ?app = app
     in void $ execute $ appMain <$ activateE
  actuate appNetwork
  exitWithInt =<< app.run Nothing

appMain :: (?app :: G.Application) => MomentIO ()
appMain = do
  window <- new G.ApplicationWindow [#application := ?app]
  window.setTitle $ Just "Transaction Signing Tool"
  window.setDefaultSize 200 200

  window.present

exitWithInt :: Int32 -> IO ()
exitWithInt 0 = exitSuccess
exitWithInt i = exitWith $ ExitFailure $ fromIntegral i
