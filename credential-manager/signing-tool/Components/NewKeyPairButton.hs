{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Components.NewKeyPairButton where

import Components.Common
import Control.Exception (catch)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)
import Crypto.PubKey.Ed25519 (generateSecretKey, toPublic)
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1Object (toASN1))
import Data.Base16.Types (extractBase16)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Data.Foldable (Foldable (fold), for_)
import Data.Functor (void, (<&>))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.GI.Base (AttrOp (..), new)
import Data.Maybe (fromMaybe, isJust)
import Data.PEM (PEM (..), pemWriteBS)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.X509 (PrivKey (..))
import GI.Gio.Interfaces.File (fileGetPath, fileNewForPath)
import GI.Gio.Objects (Cancellable)
import GI.Gtk (
  Align (..),
  ApplicationWindow,
  GError,
  Orientation (..),
  on,
 )
import GI.Gtk.Objects.Box (Box (..))
import GI.Gtk.Objects.Button (Button (..))
import GI.Gtk.Objects.Entry (Entry (..))
import GI.Gtk.Objects.FileDialog (FileDialog (..))
import GI.Gtk.Objects.Label (Label (..))
import GI.Gtk.Objects.Window (Window (..))
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (AttrOpBehavior (..), attrB, signalE0, sink)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeExtension, (<.>), (</>))
import System.Process (
  readProcessWithExitCode,
 )
import Witherable (Witherable (..))

buildNewKeyPairButton
  :: (Globals)
  => ApplicationWindow
  -> MomentIO Button
buildNewKeyPairButton appWindow = do
  btn <- new Button [#label := "New key pair"]
  clickE <- signalE0 btn #clicked
  newKeyPairE <- execute (createCSR appWindow <$ clickE)
  -- Make sure execute gets called by sending the event to some no-op sink.
  reactimate $ pure () <$ newKeyPairE
  pure btn

createCSR :: (Globals) => ApplicationWindow -> MomentIO ()
createCSR appWindow =
  runMaybeT (MaybeT . fileGetPath =<< hoistMaybe ?config.secretsDir)
    >>= \case
      Nothing -> do
        liftIO $
          showError
            appWindow
            (pure ())
            "Secrets directory not configured - please configure a secrets directory in the config file."
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
        -- let subject = "/C=NL/ST=YourState/L=YourCity/O=YourOrganization/OU=YourOrganizationalUnit/CN=YourCommonName"
        let pubKey = toPublic secretKey
        let pubKeyHash = hashWith Blake2b_224 pubKey
        let pubKeyHashHex = T.unpack $ extractBase16 $ encodeBase16 $ convert pubKeyHash
        let privateKeyFile = secretsDir </> pubKeyHashHex <.> "pem"

        box <- new Box [#orientation := OrientationVertical]
        box.setHalign AlignFill
        box.setValign AlignFill
        box.setMarginTop 8
        box.setMarginStart 8
        box.setMarginEnd 8
        box.setMarginBottom 8
        box.setSpacing 8

        label <- new Label [#label := "Create Certificate Signing Request"]
        label.setHalign AlignStart
        box.append label

        let pkHex = extractBase16 $ encodeBase16 $ convert pubKey
        pkhLabel <- new Label [#label := "Public key: " <> pkHex]
        pkhLabel.setHalign AlignStart
        box.append pkhLabel

        countryInput <-
          new
            Entry
            [ #placeholderText := "Country (2 letter code)"
            , #maxLength := 2
            ]
        countryB <- fmap nothingIfNull <$> attrB countryInput #text
        box.append countryInput

        stateInput <- new Entry [#placeholderText := "State or province (full name)"]
        stateB <- fmap nothingIfNull <$> attrB stateInput #text
        box.append stateInput

        cityInput <- new Entry [#placeholderText := "Locality (e.g. city)"]
        cityB <- fmap nothingIfNull <$> attrB cityInput #text
        box.append cityInput

        orgInput <- new Entry [#placeholderText := "Organization name"]
        orgB <- fmap nothingIfNull <$> attrB orgInput #text
        box.append orgInput

        orgDepInput <-
          new Entry [#placeholderText := "Organizational department (optional)"]
        orgDepB <- fmap nothingIfNull <$> attrB orgDepInput #text
        box.append orgDepInput

        domainInput <- new Entry [#placeholderText := "Domain name"]
        domainB <- fmap nothingIfNull <$> attrB domainInput #text
        box.append domainInput

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

        buttonBox <- new Box [#orientation := OrientationHorizontal]
        buttonBox.setHalign AlignFill
        buttonBox.setSpacing 8

        cancelButton <- new Button [#label := "Cancel"]
        cancelClickE <- signalE0 cancelButton #clicked
        reactimate $ dialog.close <$ cancelClickE
        buttonBox.append cancelButton

        let csrFieldsB =
              getCompose $
                CSRFields
                  <$> Compose countryB
                  <*> Compose stateB
                  <*> Compose cityB
                  <*> Compose orgB
                  <*> Compose (Just <$> orgDepB)
                  <*> Compose domainB

        saveButton <- new Button [#label := "Save"]
        saveClickE <- signalE0 saveButton #clicked
        let isValidB = isJust <$> csrFieldsB
        sink saveButton [#sensitive :== isValidB]
        let csrFieldsE = filterJust $ csrFieldsB <@ saveClickE
        reactimate $
          csrFieldsE <&> \CSRFields{..} -> do
            let subject =
                  T.unpack $
                    fold
                      [ "/C="
                      , country
                      , "/ST="
                      , state
                      , "/L="
                      , city
                      , "/O="
                      , org
                      , case orgDep of
                          Nothing -> ""
                          Just ou -> "/OU=" <> ou
                      , "/CN="
                      , domain
                      ]
            fileDialog <- new FileDialog [#title := "Save certificate signing request"]
            dir <- getCurrentDirectory
            currentDir <- fileNewForPath dir
            fileDialog.setInitialFolder $ Just $ fromMaybe currentDir ?config.documentsDir
            fileDialog.setInitialName $ Just "csr.pem"
            fileDialog.save (Just appWindow) (Nothing @Cancellable) $ Just \_ result -> do
              mGFile <-
                fileDialog.saveFinish result `catch` \(_ :: GError) -> do
                  -- saveFinish throws a GError if the user presses "cancel" instead of
                  -- just returning Nothing like the API would lead you to expect...
                  pure Nothing
              mFile <- wither fileGetPath mGFile
              for_ mFile \file -> do
                liftIO $ BS.writeFile privateKeyFile privateKeyPemBytes
                (exitCode, out, err) <-
                  liftIO $
                    readProcessWithExitCode
                      "openssl"
                      ["req", "-new", "-key", privateKeyFile, "-out", file, "-subj", subject]
                      ""
                case exitCode of
                  ExitSuccess -> dialog.close
                  ExitFailure i -> liftIO do
                    showError
                      appWindow
                      (pure ())
                      ("OpenSSL failed to create CSR; exit code " <> fromString (show i))
                    putStrLn out
                    putStrLn err
        buttonBox.append saveButton

        box.append buttonBox

        dialog.present

nothingIfNull :: Text -> Maybe Text
nothingIfNull t
  | T.null t = Nothing
  | otherwise = Just t

data CSRFields = CSRFields
  { country :: Text
  , state :: Text
  , city :: Text
  , org :: Text
  , orgDep :: Maybe Text
  , domain :: Text
  }

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
