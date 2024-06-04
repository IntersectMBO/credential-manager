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
     in void $ execute $ buildMainWindow <$ activateE
  actuate appNetwork
  exitWithInt =<< app.run Nothing

buildMainWindow :: (?app :: G.Application) => MomentIO ()
buildMainWindow = do
  window <- new G.ApplicationWindow [#application := ?app]
  window.setTitle $ Just "Transaction Signing Tool"

  box <- new G.Box [#orientation := G.OrientationVertical]
  box.setHalign G.AlignFill
  box.setValign G.AlignFill
  box.setMarginTop 8
  box.setMarginStart 8
  box.setMarginEnd 8
  box.setMarginBottom 8
  box.setSpacing 8
  window.setChild $ Just box

  txViewLabel <- new G.Label [#label := "Transaction summary"]
  txViewLabel.setHalign G.AlignStart
  box.append txViewLabel

  txView <- new G.ScrolledWindow []
  txView.setVexpand True
  txView.setHasFrame True
  box.append txView

  mainButtons <- buildMainButtons
  box.append mainButtons

  window.present

buildMainButtons :: MomentIO G.Box
buildMainButtons = do
  box <- new G.Box [#orientation := G.OrientationHorizontal]
  box.setHalign G.AlignStart
  box.setValign G.AlignCenter
  box.setSpacing 8

  importTxButton <- new G.Button [#label := "Import transaction"]
  box.append importTxButton

  createKeyPairButton <- new G.Button [#label := "Create new key pair"]
  box.append createKeyPairButton

  pure box

exitWithInt :: Int32 -> IO ()
exitWithInt 0 = exitSuccess
exitWithInt i = exitWith $ ExitFailure $ fromIntegral i
