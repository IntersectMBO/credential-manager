{-# LANGUAGE OverloadedStrings #-}

module OrchestratorCLI.CommandsSpec (spec, createPemFile) where

import Turtle (empty, shell)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import OrchestratorCLI.TestCommon (runOrchestratorCli, shouldCreateFiles, fileShouldExist)
import Control.Monad (void)
import Test.Hspec (Spec, around, describe, it)

-- Used as a helper elsewhere
createPemFile :: FilePath -> IO ()
createPemFile pemFile = do
  withSystemTempDirectory "pem" \dir -> do
    let skeyFile = dir </> "temp.skey"
        vkeyFile = dir </> "temp.vkey"
    void $ shell (T.pack $ "cardano-cli address key-gen --signing-key-file " ++ skeyFile ++ " --verification-key-file " ++ vkeyFile) empty
    shouldCreateFiles (runOrchestratorCli "to-pem" [T.pack skeyFile, T.pack pemFile]) [pemFile]

spec :: Spec
spec = around (withSystemTempDirectory "commands") $ do
  describe "to-pem command" $ do
    it "converts cardano-cli envelope to PEM format" $ \tempDir -> do
      let pemFile = tempDir </> "key.pem"
      createPemFile pemFile
      fileShouldExist pemFile

