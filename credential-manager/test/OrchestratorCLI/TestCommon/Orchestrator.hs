module OrchestratorCLI.TestCommon.Orchestrator where

import Control.Applicative (empty)
import qualified Data.Text.Encoding as T
import Turtle.Prelude (echo, procStrictWithErr)
import OrchestratorCLI.TestCommon.HSpecExtra (shouldExecuteSuccessfully, expectJust, shouldCreateFiles)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (listToMaybe)
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import OrchestratorCLI.TestCommon.Testnet (Testnet (..), PaymentKeyPair (..))
import System.FilePath ((</>))
import qualified Data.Text as T

data OrchestratorState = PreMinting | ColdPrepared | ColdMinted | HotPrepared | HotMinted
  deriving (Show, Eq)

data Wallet = Wallet
  { paymentKeyPair :: PaymentKeyPair
  , stakingKeyPair :: PaymentKeyPair
  }

newtype EnterpriseAddress = EnterpriseAddress Text

mkEnterpriseAddress :: Wallet -> IO EnterpriseAddress
mkEnterpriseAddress (Wallet (PaymentKeyPair _ vKeyFile) _) = do
  fmap EnterpriseAddress
    $ shouldExecuteSuccessfully
    $ procStrictWithErr "cardano-cli" ["address", "build", "--payment-verification-key-file", T.pack vKeyFile] empty

newtype Orchestrator (a :: OrchestratorState) = Orchestrator Wallet

setupOrchestrator :: FilePath -> Testnet -> IO (Orchestrator PreMinting)
setupOrchestrator workingDir (Testnet {genesisKeys=(paymentKeyPair,_,_)}) = do
  putStrLn "Setting up orchestrator..."
  putStrLn $ "Working directory: " <> workingDir
  let
    stakingSKeyFile = workingDir </> "orchestrator-stake.skey"
    stakingVKeyFile = workingDir </> "orchestrator-stake.vkey"
    stakingKeyPair = PaymentKeyPair stakingSKeyFile stakingVKeyFile
  shouldCreateFiles
    ( procStrictWithErr
      "cardano-cli"
      ["stake-address", "key-gen", "--verification-key-file", T.pack stakingVKeyFile, "--signing-key-file", T.pack stakingSKeyFile]
      empty
    )
    [stakingSKeyFile, stakingVKeyFile]
  pure $ Orchestrator $ Wallet paymentKeyPair stakingKeyPair

newtype TxOutRef = TxOutRef Text

getOrchestratorUTxO :: forall a. Orchestrator a -> IO TxOutRef
getOrchestratorUTxO (Orchestrator wallet) = do
  echo "Getting orchestrator funding UTxO..."
  EnterpriseAddress address <- mkEnterpriseAddress wallet
  jsonText <- shouldExecuteSuccessfully
    $ procStrictWithErr "cardano-cli" ["conway", "query", "utxo", "--address", address, "--output-json"] empty
  let decoded :: Maybe A.Object
      decoded = A.decode $ BL.fromStrict (T.encodeUtf8 jsonText)
      maybeKey = fmap fst . listToMaybe . KM.toList =<< decoded
  txOutRef <- K.toText <$> expectJust "Orchestrator maybeKey" maybeKey
  pure $ TxOutRef txOutRef


