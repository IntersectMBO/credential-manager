{-# LANGUAGE ApplicativeDo #-}
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
  serialiseToRawBytesHexText,
 )
import Components.Common
import Components.MainButtons (MainButtons (..), buildMainButtons)
import Components.SignTransactionButton (SignTxPlan (..))
import Components.TxView (buildTxView, createTag)
import Control.Exception (catch)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import CredentialManager.Api (parsePrivateKeyFromPEMBytes)
import Crypto.PubKey.Ed25519 (SecretKey, toPublic)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.GI.Base
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Text.IO as T
import GHC.IO.Exception (ExitCode (ExitFailure))
import GI.GLib (idleAdd, pattern PRIORITY_DEFAULT_IDLE)
import GI.Gio (fileGetPath)
import qualified GI.Gio as GIO
import GI.Gtk (TextView (..))
import qualified GI.Gtk as G
import GI.Gtk.Objects.ScrolledWindow (ScrolledWindow (..))
import GI.Gtk.Objects.TextBuffer (TextBuffer)
import Options (Options (..), options)
import Options.Applicative (execParser)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (signalE0)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (exitSuccess, exitWith)
import System.FSNotify (watchDir, withManager)
import System.FilePath ((</>))
import TxSummary (summarizeTx)
import Witherable (Witherable (..))

main :: IO ()
main = do
  Options{..} <- execParser options
  config <- loadConfig configFile
  runApp config `catch` \(e :: GError) -> do
    msg <- gerrorMessage e
    T.putStrLn msg

runApp :: Maybe (AppConfig GIO.File) -> IO ()
runApp config = do
  app <- new G.Application [#applicationId := "iog.signing-tool"]
  let ?app = app
      ?config = config
  withManager \mgr -> do
    (addHandlerMyKeys, fireMyKeys) <- newAddHandler
    initKeys <- loadKeys
    appNetwork <- compile do
      activateE <- signalE0 app #activate
      myKeysB <- fromChanges initKeys addHandlerMyKeys
      void $ execute $ buildMainWindow myKeysB <$ activateE
    actuate appNetwork
    for_ config.secretsDir \dir -> do
      fileGetPath dir >>= traverse_ \dir' ->
        watchDir mgr dir' (const True) \_ ->
          void $ idleAdd PRIORITY_DEFAULT_IDLE do
            fireMyKeys =<< loadKeys
            pure True
    exitWithInt =<< app.run Nothing

buildMainWindow :: (Globals) => Behavior [SecretKey] -> MomentIO ()
buildMainWindow myKeysB = mdo
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

  txView <- buildTxView summaryItemsB signedB
  box.append txView

  keyViewLabel <- new G.Label [#label := "My keys"]
  keyViewLabel.setHalign G.AlignStart
  box.append keyViewLabel

  keyView <- buildKeyView keysWithHashesB
  box.append keyView

  mainButtons <- buildMainButtons window do
    tx <- txB
    myKeys <- keysWithHashesB
    pure do
      txResult <- tx
      (txBodyFile, txBody) <- hush txResult
      pure SignTxPlan{..}
  signedB <-
    stepper False $
      unionWith (||) (True <$ mainButtons.signedE) (False <$ mainButtons.newTxE)
  addedKeysB <- accumB [] $ (:) <$> mainButtons.newSecretKeyE
  txB <- stepper Nothing $ Just <$> mainButtons.newTxE

  let allKeysB = myKeysB <> addedKeysB
  let keysWithHashesB = toPubKeyHashes <$> allKeysB
  let summaryItemsB = do
        keyHashes <- keysWithHashesB
        tx <- txB
        pure $ summarizeTx (Map.keysSet keyHashes) . fmap snd =<< maybeToList tx
  box.append mainButtons.widget

  window.present

buildKeyView
  :: Behavior (Map (Hash PaymentKey) SecretKey) -> MomentIO ScrolledWindow
buildKeyView keysB = do
  scrollWindow <-
    new
      ScrolledWindow
      [ #hasFrame := True
      , #minContentHeight := 250
      ]

  textView <-
    new
      TextView
      [ #editable := False
      , #cursorVisible := False
      , #leftMargin := 8
      , #rightMargin := 8
      , #topMargin := 8
      , #bottomMargin := 8
      ]
  buffer <- textView.getBuffer
  initializeBuffer buffer
  scrollWindow.setChild $ Just textView

  keys <- valueBLater keysB
  liftIOLater $ renderBuffer buffer keys
  reactimate' =<< changes (renderBuffer buffer <$> keysB)

  pure scrollWindow

renderBuffer :: TextBuffer -> Map (Hash PaymentKey) SecretKey -> IO ()
renderBuffer buffer keys = do
  (start, end) <- buffer.getBounds
  buffer.delete start end
  iter <- buffer.getIterAtOffset 0
  for_ (Map.keys keys) \pkh -> do
    buffer.insert iter (serialiseToRawBytesHexText pkh <> "\n") (-1)
  (start', end') <- buffer.getBounds
  buffer.applyTagByName "mono" start' end'

initializeBuffer :: (MonadIO m) => TextBuffer -> m ()
initializeBuffer buffer = do
  createTag
    buffer
    [ #name := "mono"
    , #family := "monospace"
    ]

toPubKeyHashes :: [SecretKey] -> Map (Hash PaymentKey) SecretKey
toPubKeyHashes = Map.fromList . fmap \k -> (toPubKeyHash k, k)

toPubKeyHash :: SecretKey -> Hash PaymentKey
toPubKeyHash =
  verificationKeyHash
    . fromRight
      ( error
          "Failed to convert from crypton public key to cardano-api verification key"
      )
    . deserialiseFromRawBytes (AsVerificationKey AsPaymentKey)
    . BA.convert
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
        wither (readPrivateKey . (dir' </>)) files

readPrivateKey :: FilePath -> IO (Maybe SecretKey)
readPrivateKey file = runMaybeT do
  exists <- liftIO $ doesFileExist file
  guard exists
  contents <- liftIO $ BS.readFile file
  hoistMaybe $ hush $ parsePrivateKeyFromPEMBytes contents

hush :: Either a b -> Maybe b
hush Left{} = Nothing
hush (Right b) = Just b
