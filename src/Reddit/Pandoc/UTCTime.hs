
module Reddit.Pandoc.UTCTime
    ( fromUTCTime
    ) where

import           Data.Time.Clock
import           Data.Time.Format
import           Text.Pandoc.Builder

fromUTCTime :: String -> UTCTime -> Inlines
fromUTCTime format = str . formatTime defaultTimeLocale format
