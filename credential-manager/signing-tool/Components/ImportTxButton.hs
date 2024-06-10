{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Components.ImportTxButton where

import Cardano.Api (
  ConwayEra,
  FileError,
  FromSomeTypeCDDL (..),
  InAnyShelleyBasedEra (..),
  ShelleyBasedEra (..),
  TextEnvelopeCddlError,
  TxBody,
  getTxBody,
  readFileTextEnvelopeCddlAnyOf,
 )
import Components.Common
import Control.Exception (catch)
import Control.Monad ((<=<))
import Data.Foldable (traverse_)
import Data.GI.Base (AttrOp (..), new)
import Data.GI.Base.GError (GError)
import Data.Maybe (fromMaybe)
import GI.Gio.Interfaces.File (fileGetPath, fileNewForPath)
import GI.Gio.Objects.Cancellable (Cancellable)
import GI.Gtk.Objects (FileDialog (..))
import GI.Gtk.Objects.ApplicationWindow (ApplicationWindow)
import GI.Gtk.Objects.Button (Button (..))
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (signalE0)
import System.Directory (getCurrentDirectory)
import Witherable (Witherable (wither))

data ImportTxButton = ImportTxButton
  { widget :: Button
  , newTxE :: Event (Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra))
  }

buildImportTxButton :: (Globals) => ApplicationWindow -> MomentIO ImportTxButton
buildImportTxButton appWindow = do
  btn <- new Button [#label := "Import transaction"]
  clickE <- signalE0 btn #clicked
  clickResultE <- execute $ importTx appWindow <$ clickE
  newTxE <- switchE never clickResultE
  pure $ ImportTxButton btn newTxE

importTx
  :: (Globals)
  => ApplicationWindow
  -> MomentIO (Event (Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra)))
importTx appWindow = do
  fileDialog <- new FileDialog [#title := "Import transaction"]
  dir <- liftIO getCurrentDirectory
  currentDir <- fileNewForPath dir
  fileDialog.setInitialFolder $ Just $ fromMaybe currentDir ?config.documentsDir
  (addHandler, fire) <- liftIO newAddHandler
  fileDialog.open (Just appWindow) (Nothing @Cancellable) $ Just \_ result -> do
    mGFile <-
      fileDialog.openFinish result `catch` \(_ :: GError) -> do
        -- openFinish throws a GError if the user presses "cancel" instead of
        -- just returning Nothing like the API would lead you to expect...
        pure Nothing
    mFile <- wither fileGetPath mGFile
    traverse_ (fire <=< readTxBodyFile) mFile
  fromAddHandler addHandler

readTxBodyFile
  :: FilePath -> IO (Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra))
readTxBodyFile =
  readFileTextEnvelopeCddlAnyOf
    [ FromCDDLTx "Unwitnessed Tx ConwayEra" \case
        InAnyShelleyBasedEra ShelleyBasedEraConway tx -> getTxBody tx
        InAnyShelleyBasedEra _ _ -> error "Unexpected shelley era"
    ]
