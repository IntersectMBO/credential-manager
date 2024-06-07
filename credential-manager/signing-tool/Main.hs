{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Components.Common
import Components.MainButtons (MainButtons (..), buildMainButtons)
import Components.TxView (buildTxView)
import Control.Exception (catch)
import Data.Functor (void)
import Data.GI.Base
import Data.Int (Int32)
import qualified Data.Text.IO as T
import GHC.IO.Exception (ExitCode (ExitFailure))
import qualified GI.Gio as GIO
import qualified GI.Gtk as G
import Options (Options (..), options)
import Options.Applicative (execParser)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (signalE0)
import System.Exit (exitSuccess, exitWith)
import TxSummary (summarizeTx)

main :: IO ()
main = do
  Options{..} <- execParser options
  config <- traverse loadConfig configFile
  runApp config `catch` \(e :: GError) -> do
    msg <- gerrorMessage e
    T.putStrLn msg

runApp :: Maybe (AppConfig GIO.File) -> IO ()
runApp config = do
  app <- new G.Application [#applicationId := "iog.signing-tool"]
  appNetwork <- compile do
    activateE <- signalE0 app #activate
    let ?app = app
        ?config = config
     in void $ execute $ buildMainWindow <$ activateE
  actuate appNetwork
  exitWithInt =<< app.run Nothing

buildMainWindow :: (Globals) => MomentIO ()
buildMainWindow = mdo
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

  txView <- buildTxView summaryItemsE
  box.append txView

  mainButtons <- buildMainButtons window
  let summaryItemsE = summarizeTx <$> mainButtons.newTxE
  box.append mainButtons.widget

  window.present

exitWithInt :: Int32 -> IO ()
exitWithInt 0 = exitSuccess
exitWithInt i = exitWith $ ExitFailure $ fromIntegral i
