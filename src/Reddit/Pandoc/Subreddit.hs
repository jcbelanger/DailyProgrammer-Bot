{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Reddit.Pandoc.Subreddit
    ( fromSubreddit
    , toR
    ) where

import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Reddit.Types.Subreddit
import           Text.Pandoc.Builder

toR :: SubredditName -> Text
toR (R subreddit) = "/r/" <> subreddit

fromSubreddit :: Subreddit -> Inlines
fromSubreddit Subreddit{..} =
    let url = "https://reddit.com/" <> Text.unpack (toR name)
        alt = "View Subreddit"
        label = (str . Text.unpack . toR) name
    in link url alt label
