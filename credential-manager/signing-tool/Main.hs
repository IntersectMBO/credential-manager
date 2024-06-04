{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.GI.Base
import Data.Int (Int32)
import GHC.IO.Exception (ExitCode (ExitFailure))
import qualified GI.Gtk as G
import System.Exit (exitSuccess, exitWith)

main :: IO ()
main = do
  G.init

  app <- new G.Application [#applicationId := "iog.signing-tool"]

  _ <- on app #activate $ activate app

  exitWithInt =<< app.run Nothing

activate :: G.Application -> IO ()
activate app = do
  window <- new G.ApplicationWindow [#application := app]
  window.setTitle $ Just "CC Transaction Signing Tool"
  window.setDefaultSize 200 200

  box <- new G.Box [#orientation := G.OrientationVertical]
  box.setHalign G.AlignCenter
  box.setValign G.AlignCenter
  window.setChild $ Just box

  button <- new G.Button [#label := "Hello World"]

  _ <- on button #clicked (putStrLn "hello")

  box.append button

  window.present

exitWithInt :: Int32 -> IO ()
exitWithInt 0 = exitSuccess
exitWithInt i = exitWith $ ExitFailure $ fromIntegral i
