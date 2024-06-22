{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Components.MainButtons where

import Cardano.Api (ConwayEra, FileError, TextEnvelopeCddlError, TxBody)
import Components.Common
import Components.ImportTxButton (ImportTxButton (..), buildImportTxButton)
import Data.GI.Base
import qualified GI.Gtk as G
import Reactive.Banana (Event)
import Reactive.Banana.Frameworks

data MainButtons = MainButtons
  { widget :: G.Box
  , newTxE :: Event (Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra))
  }

buildMainButtons :: (Globals) => G.ApplicationWindow -> MomentIO MainButtons
buildMainButtons appWindow = do
  box <- new G.Box [#orientation := G.OrientationHorizontal]
  box.setHalign G.AlignStart
  box.setValign G.AlignCenter
  box.setSpacing 8

  importTxButton <- buildImportTxButton appWindow
  box.append importTxButton.widget

  createKeyPairButton <- new G.Button [#label := "Create new key pair"]
  box.append createKeyPairButton

  pure $ MainButtons box importTxButton.newTxE
