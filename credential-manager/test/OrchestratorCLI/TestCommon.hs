module OrchestratorCLI.TestCommon
    ( runOrchestratorCli
    , runCardanoCli
    , shouldContainText
    , ignoreHookContext
    , fileShouldExist
    , shouldCreateFiles
    , shouldExecuteSuccessfully
    , createSystemTempDirectory
    , createOrchestratorWorkingDirectory
    , withOrchestartorEnv
    ) where

import Turtle (empty, ExitCode, procStrictWithErr)
import Data.Text (Text)
import OrchestratorCLI.TestCommon.HSpecExtra (ignoreHookContext, shouldCreateFiles, shouldContainText, fileShouldExist, shouldExecuteSuccessfully)
import OrchestratorCLI.TestCommon.Orchestrator (Orchestrator, setupOrchestrator, OrchestratorState(PreMinting))
import OrchestratorCLI.TestCommon.Testnet (withTestnet)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import Test.Hspec (aroundAll, Spec, aroundWith)
import Test.Hspec.Runner (SpecWith)

runOrchestratorCli :: Text -> [Text] -> IO (ExitCode, Text, Text)
runOrchestratorCli cmd args = do
    (exitCode, stdout, stderr) <- procStrictWithErr "orchestrator-cli" (cmd : args) empty
    return (exitCode, stdout, stderr)

runCardanoCli :: Text -> [Text] -> IO (ExitCode, Text, Text)
runCardanoCli cmd args = do
    (exitCode, stdout, stderr) <- procStrictWithErr "cardano-cli" (cmd : args) empty
    return (exitCode, stdout, stderr)

createSystemTempDirectory :: String -> IO FilePath
createSystemTempDirectory dirNameTemplate = do
  systemTempDir <- getCanonicalTemporaryDirectory
  createTempDirectory systemTempDir dirNameTemplate

createOrchestratorWorkingDirectory :: IO FilePath
createOrchestratorWorkingDirectory = do
  orchestratorDir <- createSystemTempDirectory "orchestrator"
  putStrLn $ "Using orchestrator directory: " <> orchestratorDir
  return orchestratorDir

-- * Creates testnet and launches it before the whole testsuite
-- * Creates orchestrator directory before each test
withOrchestartorEnv :: SpecWith (Orchestrator PreMinting, FilePath) -> Spec
withOrchestartorEnv testsuite = do
  let
    setup action testnet = do
      workingDir <- createOrchestratorWorkingDirectory
      orchestrator <- setupOrchestrator workingDir testnet
      action (orchestrator, workingDir)
  aroundAll withTestnet (aroundWith setup testsuite)



--       genesisWalletDir <- createSystemTempDirectory "genesis-wallet"
--       -- call cardano-cli
--       -- cardano-cli address key-gen --verification-key-file example/genesis-keys/genesis1.vkey --signing-key-file example/genesis-keys/genesis1.skey
--       let stakingSKey = gene
--         
--       procStrictWithErr "cardano-cli" ["address", "key-gen", "--verification-key-file", 
-- 
