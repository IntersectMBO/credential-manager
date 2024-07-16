{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecursiveDo #-}

module Components.AddSigningKeyButton where

import Components.Common
import Control.Monad ((<=<))
import Crypto.Error (maybeCryptoError)
import Crypto.PubKey.Ed25519 (SecretKey, secretKey)
import Data.ByteString.Base16 (decodeBase16Untyped)
import Data.GI.Base (AttrOp (..), new)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GI.Gtk (
  Align (..),
  ApplicationWindow,
  Entry (Entry),
  EntryIconPosition (..),
  Orientation (..),
  Window (..),
 )
import GI.Gtk.Objects (Box (..))
import GI.Gtk.Objects.Button (Button (..))
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk (
  AttrOpBehavior (..),
  attrB,
  signalE0,
  signalE1,
  sink,
 )

data AddSigningKeyButton = AddSigningKeyButton
  { widget :: Button
  , newSecretKeyE :: Event SecretKey
  }

buildAddSigningKeyButton
  :: (Globals) => ApplicationWindow -> MomentIO AddSigningKeyButton
buildAddSigningKeyButton appWindow = do
  btn <- new Button [#label := "Add Signing Key"]
  clickE <- signalE0 btn #clicked
  clickResultE <- execute $ openAddSigningKeyDialog appWindow <$ clickE
  newSecretKeyE <- switchE never clickResultE
  pure $ AddSigningKeyButton btn newSecretKeyE

openAddSigningKeyDialog
  :: (Globals)
  => ApplicationWindow
  -> MomentIO (Event SecretKey)
openAddSigningKeyDialog appWindow = mdo
  box <- new Box [#orientation := OrientationVertical]
  box.setHalign AlignFill
  box.setValign AlignFill
  box.setMarginTop 8
  box.setMarginStart 8
  box.setMarginEnd 8
  box.setMarginBottom 8
  box.setSpacing 8

  visibleB <- accumB False $ not <$ filterE isSecondary visibleClickE

  input <- new Entry [#placeholderText := "Enter your private key"]
  inputActivate <- signalE0 input #activate

  let iconB = icon <$> visibleB

  sink
    input
    [ #visibility :== visibleB
    , #secondaryIconName :== iconB
    ]

  visibleClickE <- signalE1 input #iconRelease

  textB <- attrB input #text
  let secretKeyB = parseSecretKey <$> textB

  box.append input

  buttonBox <- new Box [#orientation := OrientationHorizontal]
  buttonBox.setHalign AlignFill
  buttonBox.setSpacing 8

  cancelButton <- new Button [#label := "Cancel"]
  cancelClickE <- signalE0 cancelButton #clicked
  reactimate $ dialog.close <$ cancelClickE
  buttonBox.append cancelButton

  saveButton <- new Button [#label := "Save"]
  saveClickE <- signalE0 saveButton #clicked
  let isValidB = isJust <$> secretKeyB
  let saveE = saveClickE <> whenE isValidB inputActivate
  sink saveButton [#sensitive :== isValidB]
  let secretKeyE = filterJust $ secretKeyB <@ saveE
  reactimate $ dialog.close <$ secretKeyE
  buttonBox.append saveButton

  box.append buttonBox

  dialog <-
    new
      Window
      [ #application := ?app
      , #title := "Manually add a new signing key"
      , #modal := True
      , #transientFor := appWindow
      , #child := box
      , #defaultWidth := 800
      ]
  dialog.present
  pure secretKeyE

parseSecretKey :: Text -> Maybe SecretKey
parseSecretKey = maybeCryptoError . secretKey <=< hush . decodeBase16Untyped . encodeUtf8

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

icon :: Bool -> Text
icon False = "view-reveal-symbolic"
icon True = "view-conceal-symbolic"

isSecondary :: EntryIconPosition -> Bool
isSecondary = (== EntryIconPositionSecondary)
