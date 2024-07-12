{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Components.Common where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GI.Gio.Interfaces.File (File, fileNewForPath)
import GI.Gtk.Objects.Application (Application)
import System.Directory (doesFileExist)
import System.Exit (die)

type Globals =
  ( ?config :: Maybe (AppConfig File)
  , ?app :: Application
  )

data AppConfig f = AppConfig
  { documentsDir :: f
  , secretsDir :: f
  , defaultCountry :: Maybe Text
  , defaultState :: Maybe Text
  , defaultCity :: Maybe Text
  , defaultOrg :: Maybe Text
  , defaultOrgUnit :: Maybe Text
  }
  deriving stock (Show, Read, Ord, Eq, Generic, Functor, Foldable, Traversable)
  deriving anyclass (FromJSON, ToJSON)

loadConfig :: FilePath -> IO (Maybe (AppConfig File))
loadConfig file = runMaybeT do
  guard =<< liftIO (doesFileExist file)
  liftIO do
    result <- eitherDecodeFileStrict file
    case result of
      Left err -> die $ "Failed to load app config: " <> err
      Right config -> traverse fileNewForPath config

instance (HasField field a b) => HasField field (Maybe a) (Maybe b) where
  getField Nothing = Nothing
  getField (Just a) = Just $ getField @field a
