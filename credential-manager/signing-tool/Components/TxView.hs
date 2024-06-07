{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Components.TxView where

import Components.Common
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.GI.Base
import Data.GI.Base.Attributes (AttrOpTag (..))
import Data.Text (Text)
import qualified Data.Text as T
import GI.Gtk (
  ScrolledWindow (..),
  TextBuffer,
  TextIter,
  TextTag (TextTag),
  TextView (..),
 )
import Reactive.Banana (Event)
import Reactive.Banana.Frameworks
import TxSummary.Common (ItemStatus (..), SummaryItem (..))

buildTxView :: (Globals) => Event [SummaryItem] -> MomentIO ScrolledWindow
buildTxView summaryItemsE = do
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

  reactimate $ renderBuffer buffer <$> summaryItemsE

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
    , #foreground := "goldenrod"
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

renderBuffer :: TextBuffer -> [SummaryItem] -> IO ()
renderBuffer buffer items = do
  (start, end) <- buffer.getBounds
  buffer.delete start end
  iter <- buffer.getIterAtOffset 0
  traverse_ (renderItem buffer iter) items
  (start', end') <- buffer.getBounds
  buffer.applyTagByName "mono" start' end'

renderItem :: TextBuffer -> TextIter -> SummaryItem -> IO ()
renderItem buffer iter SummaryItem{..} = do
  let (icon, titleTag) = case itemStatus of
        Ok -> ("✓", "step-ok")
        Warn -> ("⚠", "step-warn")
        Error -> ("✘", "step-error")
  insertWithTagsByName
    buffer
    iter
    (icon <> " " <> itemTitle <> "\n")
    [titleTag, "step"]
  insertWithTagsByName buffer iter (T.unlines itemDetails) ["step-details"]

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
