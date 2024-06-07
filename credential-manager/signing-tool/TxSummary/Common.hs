module TxSummary.Common where

import Data.Text (Text)

data SummaryItem = SummaryItem
  { itemStatus :: ItemStatus
  , itemTitle :: Text
  , itemDetails :: [Text]
  }

data ItemStatus = Ok | Warn | Error
