module TxSummary.Common where

import Cardano.Api (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer (Writer, runWriter, tell, writer)
import Data.Text (Text)

data SummaryItem = SummaryItem
  { itemStatus :: ItemStatus
  , itemTitle :: Text
  , itemDetails :: [Text]
  }

data ItemStatus = Ok | Warn | Error

instance Semigroup ItemStatus where
  Ok <> b = b
  a <> Ok = a
  Error <> _ = Error
  _ <> Error = Error
  Warn <> Warn = Warn

instance Monoid ItemStatus where
  mempty = Ok

type SummaryM = MaybeT (Writer [SummaryItem])
type ItemM = MaybeT (Writer (ItemStatus, [Text]))

summarize :: Text -> ItemM a -> SummaryM a
summarize itemTitle m = MaybeT $ writer (ma, [SummaryItem{..}])
  where
    (ma, (itemStatus, itemDetails)) = runWriter $ runMaybeT m

warnStatus :: ItemM ()
warnStatus = lift $ tell (Warn, [])

errorStatus :: ItemM ()
errorStatus = lift $ tell (Error, [])

describe :: Text -> ItemM ()
describe line = lift $ tell (Ok, [line])
