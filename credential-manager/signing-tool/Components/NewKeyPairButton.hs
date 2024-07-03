{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Components.NewKeyPairButton where

import Components.Common
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)
import Crypto.PubKey.Ed25519 (SecretKey, generateSecretKey, toPublic)
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1Object (toASN1))
import Data.Base16.Types (extractBase16)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Data.Functor (void)
import Data.GI.Base (AttrOp (..), new)
import Data.PEM (PEM (..), pemWriteBS)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.X509 (PrivKey (..))
import GI.Gio.Interfaces.File (fileGetPath)
import GI.Gtk (
  Align (..),
  ApplicationWindow,
  Orientation (..),
  on,
 )
import GI.Gtk.Objects.Box (Box (..))
import GI.Gtk.Objects.Button (Button (..))
import GI.Gtk.Objects.Label (Label (..))
import GI.Gtk.Objects.Window (Window (..))
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (signalE0)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeExtension, (<.>), (</>))
import System.Process (
  readProcessWithExitCode,
 )

data NewKeyPairButton = NewKeyPairButton
  { widget :: Button
  , newKeyPairE :: Event SecretKey
  }

buildNewKeyPairButton
  :: (Globals)
  => ApplicationWindow
  -> MomentIO NewKeyPairButton
buildNewKeyPairButton appWindow = do
  btn <- new Button [#label := "New key pair"]
  clickE <- signalE0 btn #clicked
  newKeyPairE <- switchE never =<< execute (createCSR appWindow <$ clickE)
  pure $ NewKeyPairButton btn newKeyPairE

createCSR :: (Globals) => ApplicationWindow -> MomentIO (Event SecretKey)
createCSR appWindow =
  runMaybeT (MaybeT . fileGetPath =<< hoistMaybe ?config.secretsDir)
    >>= \case
      Nothing -> do
        liftIO $
          showError
            appWindow
            (pure ())
            "Secrets directory not configured - please configure a secrets directory in the config file."
        pure never
      Just secretsDir -> do
        secretKey <- liftIO generateSecretKey
        let asn1 = toASN1 (PrivKeyEd25519 secretKey) []
        let pemContent = encodeASN1' DER asn1
        let pem =
              PEM
                { pemName = "PRIVATE KEY"
                , pemHeader = []
                , pemContent
                }
        let privateKeyPemBytes = pemWriteBS pem
        let subject =
              "/C=NL/ST=YourState/L=YourCity/O=YourOrganization/OU=YourOrganizationalUnit/CN=YourCommonName"
        let pubKey = toPublic secretKey
        let pubKeyHash = hashWith Blake2b_224 pubKey
        let pubKeyHashHex = T.unpack $ extractBase16 $ encodeBase16 $ convert pubKeyHash
        let privateKeyFile = secretsDir </> pubKeyHashHex <.> "pem"
        let csrFile = "csr.pem"
        liftIO $ BS.writeFile privateKeyFile privateKeyPemBytes
        (exitCode, out, err) <-
          liftIO $
            readProcessWithExitCode
              "openssl"
              ["req", "-new", "-key", privateKeyFile, "-out", csrFile, "-subj", subject]
              ""
        case exitCode of
          ExitSuccess -> pure never
          ExitFailure i -> liftIO do
            showError
              appWindow
              (pure ())
              ("OpenSSL failed to create CSR; exit code " <> fromString (show i))
            putStrLn out
            putStrLn err
            pure never

-- savePrivateKey
--   :: (Globals)
--   => Handler ()
--   -> ApplicationWindow
--   -> IO ()
-- savePrivateKey fire appWindow = mdo
--   case ?config.secretsDir of
--     Nothing ->
--       showError
--         appWindow
--         (pure ())
--         "Secrets directory not configured - please configure a secrets directory in the config file."
--     Just dir -> do
--       fileDialog <- new FileDialog [#title := "Save private key file"]
--       Just dir' <- fileGetPath dir
--       name <- uniquifyName dir' "private.pem"
--       fileDialog.setInitialFolder $ Just dir
--       fileDialog.setInitialName $ Just $ T.pack name
--       fileDialog.save (Just appWindow) (Nothing @Cancellable) $ Just \_ result -> do
--         mGFile <-
--           fileDialog.saveFinish result `catch` \(_ :: GError) -> do
--             -- saveFinish throws a GError if the user presses "cancel" instead of
--             -- just returning Nothing like the API would lead you to expect...
--             pure Nothing
--         mFile <- wither fileGetPath mGFile
--         for_ mFile \file -> do
--           exists <- doesFileExist file
--           when exists $
--             showError
--               appWindow
--               (savePrivateKey fire appWindow)
--               "Refusing to overwrite private key file."
--           callProcess "openssl" ["genpkey", "-algorithm", "ed25519", "-out", file]
--           saveCSR fire appWindow file

showError
  :: (Globals)
  => ApplicationWindow
  -> IO ()
  -> Text
  -> IO ()
showError appWindow next msg = do
  box <- new Box [#orientation := OrientationVertical]
  box.setHalign AlignFill
  box.setValign AlignFill
  box.setMarginTop 8
  box.setMarginStart 8
  box.setMarginEnd 8
  box.setMarginBottom 8
  box.setSpacing 8

  label <- new Label [#label := msg]
  label.setHalign AlignStart
  box.append label

  okButton <- new Button [#label := "OK"]
  box.append okButton

  dialog <-
    new
      Window
      [ #application := ?app
      , #title := "Confirm signatures"
      , #modal := True
      , #transientFor := appWindow
      , #child := box
      , #defaultWidth := 800
      ]
  void $ on okButton #clicked $ dialog.close *> next
  dialog.present

uniquifyName :: FilePath -> FilePath -> IO FilePath
uniquifyName dir name = go (0 :: Int)
  where
    go i = do
      let name'
            | i == 0 = name
            | otherwise = takeBaseName name <> "(" <> show i <> ")" <.> takeExtension name
      exists <- doesFileExist $ dir </> name'
      if exists then go (succ i) else pure name'
