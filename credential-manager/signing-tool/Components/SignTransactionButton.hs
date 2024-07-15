{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}

module Components.SignTransactionButton where

import Cardano.Api (
  AsType (..),
  ConwayEra,
  File (..),
  Hash,
  PaymentKey,
  ShelleyBasedEra (ShelleyBasedEraConway),
  ShelleyWitnessSigningKey (..),
  TxBody (..),
  TxBodyContent (..),
  TxExtraKeyWitnesses (..),
  getTxId,
  makeShelleyKeyWitness,
  serialiseToRawBytesHexText,
  writeTxWitnessFileTextEnvelopeCddl,
 )
import Cardano.Api.Shelley (deserialiseFromRawBytes)
import Components.Common
import Control.Exception (catch)
import Control.Monad (guard)
import Crypto.PubKey.Ed25519 (SecretKey)
import qualified Data.ByteArray as BA
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.GI.Base (AttrOp (..), new)
import Data.GI.Base.GError (GError)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import GI.Gio.Interfaces.File (fileGetPath, fileNewForPath)
import GI.Gio.Objects.Cancellable (Cancellable)
import GI.Gtk (
  Align (..),
  ApplicationWindow,
  Label (..),
  Orientation (..),
  Window (..),
 )
import GI.Gtk.Objects (Box (..), FileDialog (..))
import GI.Gtk.Objects.Button (Button (..))
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (AttrOpBehavior (..), signalE0, sink)
import System.FilePath (takeDirectory)
import System.IO (hPrint, stderr)
import Witherable (Witherable (wither))

data SignTransactionButton = SignTransactionButton
  { widget :: Button
  , signedE :: Event ()
  }

data SignTxPlan = SignTxPlan
  { txBody :: TxBody ConwayEra
  , txBodyFile :: FilePath
  , myKeys :: Map (Hash PaymentKey) SecretKey
  }

buildSignTransactionButton
  :: (Globals)
  => ApplicationWindow
  -> Behavior (Maybe SignTxPlan)
  -> MomentIO SignTransactionButton
buildSignTransactionButton appWindow signTxPlanB = do
  btn <- new Button [#label := "Sign transaction"]
  let validPlanB = (validatePlan =<<) <$> signTxPlanB
  sink btn [#sensitive :== isJust <$> validPlanB]
  clickE <- signalE0 btn #clicked
  let clickValidE = filterJust $ validPlanB <@ clickE
  confirmedEE <- execute $ openSignTransactionDialog appWindow <$> clickValidE
  confirmedE <- switchE never confirmedEE
  signedEE <- execute $ openSaveDialogs appWindow <$> confirmedE
  signedE <- switchE never signedEE
  pure $ SignTransactionButton btn signedE

validatePlan :: SignTxPlan -> Maybe SignTxPlan
validatePlan SignTxPlan{..} = do
  let TxBody TxBodyContent{..} = txBody
  case txExtraKeyWits of
    TxExtraKeyWitnessesNone -> Nothing
    TxExtraKeyWitnesses _ requiredSigners -> do
      let filteredKeys = Map.restrictKeys myKeys $ Set.fromList requiredSigners
      guard $ not $ Map.null filteredKeys
      pure SignTxPlan{myKeys = filteredKeys, ..}

openSignTransactionDialog
  :: (Globals)
  => ApplicationWindow
  -> SignTxPlan
  -> MomentIO (Event SignTxPlan)
openSignTransactionDialog appWindow SignTxPlan{..} = mdo
  box <- new Box [#orientation := OrientationVertical]
  box.setHalign AlignFill
  box.setValign AlignFill
  box.setMarginTop 8
  box.setMarginStart 8
  box.setMarginEnd 8
  box.setMarginBottom 8
  box.setSpacing 8

  label <-
    new Label [#label := "Signing transaction with the following key hashes:"]
  label.setHalign AlignStart
  box.append label

  for_ (Map.keys myKeys) \pkh -> do
    label' <- new Label [#label := serialiseToRawBytesHexText pkh]
    label'.setHalign AlignStart
    box.append label'

  buttonBox <- new Box [#orientation := OrientationHorizontal]
  buttonBox.setHalign AlignFill
  buttonBox.setSpacing 8

  cancelButton <- new Button [#label := "Cancel"]
  cancelClickE <- signalE0 cancelButton #clicked
  reactimate $ dialog.close <$ cancelClickE
  buttonBox.append cancelButton

  confirmButton <- new Button [#label := "Confirm"]
  confirmClickE <- signalE0 confirmButton #clicked
  reactimate $ dialog.close <$ confirmClickE
  buttonBox.append confirmButton

  box.append buttonBox

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
  dialog.present
  pure $ SignTxPlan{..} <$ confirmClickE

openSaveDialogs
  :: ApplicationWindow
  -> SignTxPlan
  -> MomentIO (Event ())
openSaveDialogs appWindow SignTxPlan{..} = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ go fire $ Map.toList myKeys
  fromAddHandler addHandler
  where
    go fire [] = fire ()
    go fire ((pkh, key) : tl) = do
      fileDialog <- new FileDialog [#title := "Save signature"]
      txBodyDir <- fileNewForPath $ takeDirectory txBodyFile
      fileDialog.setInitialFolder $ Just txBodyDir
      fileDialog.setInitialName $
        Just $
          serialiseToRawBytesHexText (getTxId txBody)
            <> "."
            <> serialiseToRawBytesHexText pkh
            <> "."
            <> "witness"
      fileDialog.save (Just appWindow) (Nothing @Cancellable) $ Just \_ result -> do
        mGFile <-
          fileDialog.saveFinish result `catch` \(_ :: GError) -> do
            -- saveFinish throws a GError if the user presses "cancel" instead of
            -- just returning Nothing like the API would lead you to expect...
            pure Nothing
        mFile <- wither fileGetPath mGFile
        for_ mFile \file -> do
          let witness =
                makeShelleyKeyWitness
                  ShelleyBasedEraConway
                  txBody
                  . WitnessPaymentKey
                  . fromRight
                    ( error
                        "Failed to convert from crypton private key to cardano-api signing key"
                    )
                  . deserialiseFromRawBytes (AsSigningKey AsPaymentKey)
                  . BA.convert
                  $ key
          saveResult <-
            writeTxWitnessFileTextEnvelopeCddl
              ShelleyBasedEraConway
              (File file)
              witness
          case saveResult of
            Left err -> hPrint stderr err
            Right _ -> go fire tl
