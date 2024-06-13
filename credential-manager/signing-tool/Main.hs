{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api (
  AsType (..),
  Hash,
  Key (verificationKeyHash),
  PaymentKey,
  SerialiseAsRawBytes (deserialiseFromRawBytes),
 )
import Components.Common
import Components.MainButtons (MainButtons (..), buildMainButtons)
import Components.TxView (buildTxView)
import Control.Exception (catch)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import CredentialManager.Api (parsePrivateKeyFromPEMBytes)
import Crypto.PubKey.Ed25519 (SecretKey, toPublic)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Functor (void)
import Data.GI.Base
import Data.Int (Int32)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import GHC.IO.Exception (ExitCode (ExitFailure))
import GI.Gio (fileGetPath)
import qualified GI.Gio as GIO
import qualified GI.Gtk as G
import Options (Options (..), options)
import Options.Applicative (execParser)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (signalE0)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (exitSuccess, exitWith)
import TxSummary (summarizeTx)
import Witherable (Witherable (..))

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
  initialKeys <- liftIO loadKeys
  myKeysB <-
    stepper initialKeys =<< mapEventIO (const loadKeys) mainButtons.newTxE
  let keyHashesB = toPubKeyHashes <$> myKeysB
  let summaryItemsE = summarizeTx <$> keyHashesB <@> mainButtons.newTxE
  box.append mainButtons.widget

  window.present

toPubKeyHashes :: [SecretKey] -> Set (Hash PaymentKey)
toPubKeyHashes = Set.fromList . fmap toPubKeyHash

toPubKeyHash :: SecretKey -> Hash PaymentKey
toPubKeyHash =
  verificationKeyHash
    . fromRight
      ( error
          "Failed to convert from crypton public key to cardano-api verification key"
      )
    . deserialiseFromRawBytes (AsVerificationKey AsPaymentKey)
    . BS.pack
    . BA.unpack
    . toPublic

exitWithInt :: Int32 -> IO ()
exitWithInt 0 = exitSuccess
exitWithInt i = exitWith $ ExitFailure $ fromIntegral i

loadKeys :: (Globals) => IO [SecretKey]
loadKeys = case ?config.secretsDir of
  Nothing -> pure mempty
  Just secretsDir -> do
    dir <- fileGetPath secretsDir
    case dir of
      Nothing -> pure mempty
      Just dir' -> do
        files <- listDirectory dir'
        wither readPrivateKey files

readPrivateKey :: FilePath -> IO (Maybe SecretKey)
readPrivateKey file = runMaybeT do
  exists <- liftIO $ doesFileExist file
  guard exists
  contents <- liftIO $ BS.readFile file
  hoistMaybe $ hush $ parsePrivateKeyFromPEMBytes contents

hush :: Either a b -> Maybe b
hush Left{} = Nothing
hush (Right b) = Just b
