{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Components.NewCredentialButton where

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
import Data.Foldable (Foldable (fold), for_, traverse_)
import Data.Functor (void, (<&>))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.GI.Base (AttrOp (..), new)
import Data.GI.Base.Attributes (AttrOpTag (..))
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

buildNewCredentialButton
  :: (Globals)
  => ApplicationWindow
  -> MomentIO Button
buildNewCredentialButton appWindow = do
  btn <- new Button [#label := "New credential"]
  clickE <- signalE0 btn #clicked
  newCredentialE <- execute (createCSR appWindow <$ clickE)
  -- Make sure execute gets called by sending the event to some no-op sink.
  reactimate $ pure () <$ newCredentialE
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

        label <- new Label []
        label.setMarkup "<b>New Certificate Signing Request</b>"
        label.setHalign AlignStart
        box.append label

        let pkHex = extractBase16 $ encodeBase16 $ convert pubKey
        pkLabel <- new Label [#halign := AlignStart]
        pkLabel.setMarkup "<small>Public key</small>"
        pkEntry <- new Entry [#text := pkHex, #editable := False, #sensitive := False]
        box.append pkLabel
        box.append pkEntry

        countryB <-
          input
            box
            defaultCountry
            "Country code (optional)"
            "2 letter code"
            [#maxLength := 2]

        stateB <- input box defaultState "State (optional)" "full name" []
        cityB <- input box defaultCity "Locality (optional)" "e.g. city" []
        orgB <- input box defaultOrg "Organization name (optional)" "legal name" []
        orgUnitB <-
          input box defaultOrgUnit "Organizational department (optional)" "" []
        nameB <- input box (const Nothing) "Role (required)" "e.g. Membership" []

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
                  <$> Compose (Just <$> countryB)
                  <*> Compose (Just <$> stateB)
                  <*> Compose (Just <$> cityB)
                  <*> Compose (Just <$> orgB)
                  <*> Compose (Just <$> orgUnitB)
                  <*> Compose nameB

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
                      [ case country of
                          Nothing -> ""
                          Just c -> "/C=" <> c
                      , case state of
                          Nothing -> ""
                          Just st -> "/ST=" <> st
                      , case city of
                          Nothing -> ""
                          Just l -> "/L=" <> l
                      , case org of
                          Nothing -> ""
                          Just o -> "/O=" <> o
                      , case orgUnit of
                          Nothing -> ""
                          Just ou -> "/OU=" <> ou
                      , "/CN="
                      , name
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

input
  :: (Globals)
  => Box
  -> (forall a. AppConfig a -> Maybe Text)
  -> Text
  -> Text
  -> [AttrOp Entry AttrConstruct]
  -> MomentIO (Behavior (Maybe Text))
input box getter labelText placeholderText extraAttrs = do
  label <- new Label [#halign := AlignStart]
  label.setMarkup $ "<small>" <> labelText <> "</small>"
  entry <- new Entry $ (#placeholderText := placeholderText) : extraAttrs
  traverse_ entry.setText $ getter =<< ?config
  box.append label
  box.append entry
  fmap nothingIfNull <$> attrB entry #text

nothingIfNull :: Text -> Maybe Text
nothingIfNull t
  | T.null t = Nothing
  | otherwise = Just t

data CSRFields = CSRFields
  { country :: Maybe Text
  , state :: Maybe Text
  , city :: Maybe Text
  , org :: Maybe Text
  , orgUnit :: Maybe Text
  , name :: Text
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
