{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Components.Common where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GI.Gio.Interfaces.File (File, fileNewForPath)
import GI.Gtk.Objects.Application (Application)
import System.Exit (die)

type Globals =
  ( ?config :: Maybe (AppConfig File)
  , ?app :: Application
  )

data AppConfig f = AppConfig
  { documentsDir :: f
  , secretsDir :: f
  }
  deriving stock (Show, Read, Ord, Eq, Generic, Functor, Foldable, Traversable)
  deriving anyclass (FromJSON, ToJSON)

loadConfig :: FilePath -> IO (AppConfig File)
loadConfig file = do
  result <- eitherDecodeFileStrict file
  case result of
    Left err -> die $ "Failed to load app config: " <> err
    Right config -> traverse fileNewForPath config

instance (HasField field a b) => HasField field (Maybe a) (Maybe b) where
  getField Nothing = Nothing
  getField (Just a) = Just $ getField @field a