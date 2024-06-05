{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Components.TxView where

import Cardano.Api (
  ConwayEra,
  FileError (..),
  TextEnvelopeCddlError (..),
  TxBody (..),
  TxBodyContent (..),
 )
import Cardano.Binary (DecoderError)
import Components.Common
import Control.Exception (Exception (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (for_)
import Data.Functor (void)
import Data.GI.Base
import Data.GI.Base.Attributes (AttrOpTag (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Formatting.Buildable (Buildable (..))
import GI.Gtk (
  ScrolledWindow (..),
  TextBuffer,
  TextIter,
  TextTag (TextTag),
  TextView (..),
 )
import Reactive.Banana (Event)
import Reactive.Banana.Frameworks

buildTxView
  :: (Globals)
  => Event (Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra))
  -> MomentIO ScrolledWindow
buildTxView newTxE = do
  scrollWindow <-
    new
      ScrolledWindow
      [ #vexpand := True
      , #hasFrame := True
      ]

  textView <-
    new
      TextView
      [ #editable := False
      , #cursorVisible := False
      , #leftMargin := 8
      , #rightMargin := 8
      , #topMargin := 8
      , #bottomMargin := 8
      ]
  buffer <- textView.getBuffer
  initializeBuffer buffer
  scrollWindow.setChild $ Just textView

  reactimate $ renderBuffer buffer <$> newTxE

  pure scrollWindow

initializeBuffer :: (MonadIO m) => TextBuffer -> m ()
initializeBuffer buffer = do
  createTag
    buffer
    [ #name := "mono"
    , #family := "monospace"
    ]
  createTag
    buffer
    [ #name := "step"
    , #weight := 900
    ]
  createTag
    buffer
    [ #name := "step-details"
    , #foreground := "gray"
    , #indent := 16
    ]
  createTag
    buffer
    [ #name := "indent"
    , #indent := 32
    ]
  createTag
    buffer
    [ #name := "step-ok"
    , #foreground := "green"
    ]
  createTag
    buffer
    [ #name := "step-warn"
    , #foreground := "yellow"
    ]
  createTag
    buffer
    [ #name := "step-error"
    , #foreground := "red"
    ]

createTag
  :: (MonadIO m) => TextBuffer -> [AttrOp TextTag 'AttrConstruct] -> m ()
createTag buffer attrs = do
  tbl <- buffer.getTagTable
  tag <- new TextTag attrs
  void $ tbl.add tag

renderBuffer
  :: TextBuffer
  -> Either (FileError TextEnvelopeCddlError) (TxBody ConwayEra)
  -> IO ()
renderBuffer buffer state = do
  (start, end) <- buffer.getBounds
  buffer.delete start end
  iter <- buffer.getIterAtOffset 0
  void $ runMaybeT do
    tx <- insertStep buffer iter "Check transaction body file" do
      case state of
        Left err -> renderError buffer iter err
        Right tx -> pure (Ok tx)
    renderTx buffer iter tx
  (start', end') <- buffer.getBounds
  buffer.applyTagByName "mono" start' end'

renderError
  :: TextBuffer -> TextIter -> FileError TextEnvelopeCddlError -> IO (StepResult a)
renderError buffer iter err = do
  case err of
    FileError _ (TextEnvelopeCddlErrUnknownType t) -> do
      buffer.insert
        iter
        ("Text envelope \"type\" field invalid. Unknown type: " <> t <> "\n")
        (-1)
    FileError _ (TextEnvelopeCddlTypeError expected received) ->
      renderTextEnvelopeTypeError buffer iter expected received
    FileError _ (TextEnvelopeCddlErrCBORDecodingError decodeError) ->
      renderTextEnvelopeDecodeError buffer iter decodeError
    FileError _ (TextEnvelopeCddlAesonDecodeError _ decodeError) ->
      renderTextEnvelopeAesonDecodeError buffer iter decodeError
    FileError _ TextEnvelopeCddlUnknownKeyWitness -> pure () -- won't happen here
    FileError _ TextEnvelopeCddlErrByronKeyWitnessUnsupported -> pure () -- won't happen here
    FileErrorTempFile{} -> do
      buffer.insert iter "Unable to open temporary file\n" (-1)
    FileDoesNotExistError{} -> do
      buffer.insert iter "File does not exist\n" (-1)
    FileIOError _ ioErr -> do
      buffer.insert iter "Error reading file:\n" (-1)
      buffer.insert iter (T.pack $ displayException ioErr) (-1)
  pure Error

renderTextEnvelopeTypeError
  :: TextBuffer
  -> TextIter
  -> [Text]
  -> Text
  -> IO ()
renderTextEnvelopeTypeError buffer iter expected received = do
  buffer.insert
    iter
    "Text envelope \"type\" field invalid. Expected one of:\n"
    (-1)
  renderWithTagsByName buffer iter ["indent"] do
    for_ expected \t -> buffer.insert iter ("⋅ " <> t <> "\n") (-1)
  buffer.insert iter "But got " (-1)
  buffer.insert iter received (-1)

renderTextEnvelopeDecodeError :: TextBuffer -> TextIter -> DecoderError -> IO ()
renderTextEnvelopeDecodeError buffer iter err = do
  buffer.insert
    iter
    "Text envelope file contains invalid tx body binary data. Details:\n"
    (-1)
  renderWithTagsByName buffer iter ["indent"] do
    buffer.insert iter (TL.toStrict $ TLB.toLazyText $ build err) (-1)

renderTextEnvelopeAesonDecodeError :: TextBuffer -> TextIter -> String -> IO ()
renderTextEnvelopeAesonDecodeError buffer iter err = do
  buffer.insert iter "Text envelope file contains invalid JSON. Details:\n" (-1)
  buffer.insert iter (T.pack err) (-1)

renderTx :: TextBuffer -> TextIter -> TxBody ConwayEra -> MaybeT IO ()
renderTx _buffer _iter (TxBody TxBodyContent{}) = pure ()

renderWithTagsByName :: TextBuffer -> TextIter -> [Text] -> IO a -> IO a
renderWithTagsByName buffer iter tags render = do
  start <- buffer.createMark Nothing iter True
  a <- render
  startIter <- buffer.getIterAtMark start
  for_ tags \tag -> buffer.applyTagByName tag startIter iter
  pure a

insertWithTagsByName :: TextBuffer -> TextIter -> Text -> [Text] -> IO ()
insertWithTagsByName buffer iter text tags = do
  renderWithTagsByName buffer iter tags (buffer.insert iter text (-1))

data StepResult a = Ok a | Warn a | Error

insertStep :: TextBuffer -> TextIter -> Text -> IO (StepResult a) -> MaybeT IO a
insertStep buffer iter title step = MaybeT do
  titleStart <- buffer.createMark Nothing iter True
  buffer.insert iter title (-1)
  titleEnd <- buffer.createMark Nothing iter True
  buffer.insert iter "\n" (-1)
  result <- renderWithTagsByName buffer iter ["step-details"] step
  let (icon, titleTag, mResult) = case result of
        Ok a -> ("✓", "step-ok", Just a)
        Warn a -> ("⚠", "step-warn", Just a)
        Error -> ("✘", "step-error", Nothing)
  titleStartIter <- buffer.getIterAtMark titleStart
  buffer.insert titleStartIter (icon <> " ") (-1)
  titleStartIter' <- buffer.getIterAtMark titleStart
  titleEndIter <- buffer.getIterAtMark titleEnd
  buffer.applyTagByName "step" titleStartIter' titleEndIter
  buffer.applyTagByName titleTag titleStartIter' titleEndIter
  pure mResult
