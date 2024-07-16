{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Components.MainButtons where

import Cardano.Api (ConwayEra, FileError, TextEnvelopeCddlError, TxBody)
import Components.AddSigningKeyButton
import Components.Common
import Components.ImportTxButton (ImportTxButton (..), buildImportTxButton)
import Components.NewCredentialButton
import Components.SignTransactionButton
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.GI.Base
import qualified GI.Gtk as G
import Reactive.Banana (Behavior, Event)
import Reactive.Banana.Frameworks

data MainButtons = MainButtons
  { widget :: G.Box
  , newTxE
      :: Event (Either (FileError TextEnvelopeCddlError) (FilePath, TxBody ConwayEra))
  , newSecretKeyE :: Event SecretKey
  , signedE :: Event ()
  }

buildMainButtons
  :: (Globals)
  => G.ApplicationWindow
  -> Behavior (Maybe SignTxPlan)
  -> MomentIO MainButtons
buildMainButtons appWindow signTxPlanB = do
  box <- new G.Box [#orientation := G.OrientationHorizontal]
  box.setHalign G.AlignStart
  box.setValign G.AlignCenter
  box.setSpacing 8

  importTxButton <- buildImportTxButton appWindow
  box.append importTxButton.widget

  createKeyPairButton <- buildNewCredentialButton appWindow
  box.append createKeyPairButton

  addSigningKeyButton <- buildAddSigningKeyButton appWindow
  box.append addSigningKeyButton.widget

  signTransactionButton <- buildSignTransactionButton appWindow signTxPlanB
  box.append signTransactionButton.widget

  pure $
    MainButtons
      box
      importTxButton.newTxE
      addSigningKeyButton.newSecretKeyE
      signTransactionButton.signedE
