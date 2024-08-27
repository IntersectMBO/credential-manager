{-# LANGUAGE OverloadedStrings #-}

module OrchestratorCLI.TestCommon.Testnet where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Monad.Extra (loopM)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import OrchestratorCLI.TestCommon.Turtle (procGroupToFiles, halt)
import Control.Exception.Safe (bracket)
import Test.HUnit (assertFailure)
import Control.Concurrent (threadDelay)
import Turtle (ExitCode (..))
import Turtle.Prelude ( echo, which, procStrictWithErr, export )
import Data.Functor ((<&>))
import System.FilePath ((</>))
import Data.String (IsString(..))
import Control.Applicative (Alternative(empty))
import Control.Monad (void, when)
import OrchestratorCLI.TestCommon.HSpecExtra (expectJust, getEnvVar, fileShouldExist)
import Text.Read (readMaybe)

data PaymentKeyPair = PaymentKeyPair
  { sKeyFile :: FilePath
  , vKeyFile :: FilePath
  }

data Testnet = Testnet
  { runner :: Async ExitCode
  , root :: FilePath
  , magic :: Int
  , genesisKeys :: (PaymentKeyPair, PaymentKeyPair, PaymentKeyPair)
  }

initTestnet :: IO Testnet
initTestnet = do
    echo "Initializing testnet..."
    datePath <- which "date" <&> fromMaybe "date"
    export "DATE" (T.pack datePath)
    uname <- which "uname" <&> fromMaybe "uname"
    export "UNAME" (T.pack uname)
    testnetDir <- getEnvVar "TESTNET_DIR"
    testnetMagic <- do
      v <- getEnvVar "CARDANO_NODE_NETWORK_ID"
      expectJust "Failed parsing testnet magic" $ readMaybe v

    testnetRunner <- async $ do
      echo "Starting testnet..."
      systemTempDir <- getCanonicalTemporaryDirectory
      tempDir <- createTempDirectory systemTempDir "testnet"
      let
        stdoutFile = tempDir </> "orchestrator-testnet-stdout-.log"
        stderrFile = tempDir </> "orchestrator-testnet-stderr-.log"
      echo $ fromString $ "Logging the testnet nodes output to " <> stdoutFile <> " and " <> stderrFile
      procGroupToFiles "deploy-local-testnet" [] empty stdoutFile stderrFile
    void $ flip loopM (20 :: Int) \i -> do
      putStrLn $ "Trying to start testnet, attempt " ++ show i
      when (i == 0) $ assertFailure "Failed to start testnet"
      (exitCode, _, _) <- procStrictWithErr "cardano-cli" ["query", "tip"] empty
      case exitCode of
        ExitSuccess -> pure $ Right ()
        _ -> do
          threadDelay 1000000
          return $ Left (i -1)
    let
      mkGenesisWallet :: Int -> IO PaymentKeyPair
      mkGenesisWallet ix = do
        let keysDir = testnetDir </> "example" </> "utxo-keys"
            paymentSKey = keysDir </> "utxo" <> show ix <> ".skey"
            paymentVKey = keysDir </> "utxo" <> show ix <> ".vkey"
        fileShouldExist paymentSKey
        fileShouldExist paymentVKey
        pure $ PaymentKeyPair paymentSKey paymentVKey
    keys <- (,,) <$> mkGenesisWallet 1 <*> mkGenesisWallet 2 <*> mkGenesisWallet 3
    pure $ Testnet testnetRunner testnetDir testnetMagic keys

-- Cleanup testnet
cleanupTestnet :: Testnet -> IO ()
cleanupTestnet (Testnet { runner }) = do
  halt runner
  echo "Cleaning up testnet..."
  void $ waitCatch runner

withTestnet :: (Testnet  -> IO a) -> IO a
withTestnet = bracket initTestnet cleanupTestnet

