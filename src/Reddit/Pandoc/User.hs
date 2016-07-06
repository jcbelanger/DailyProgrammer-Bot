{-# LANGUAGE OverloadedStrings #-}

module Reddit.Pandoc.User
    ( fromUsername
    ) where

import qualified Data.Text           as Text
import           Reddit.Types.User
import           Text.Pandoc.Builder

fromUsername :: Username -> Inlines
fromUsername (Username user) =
    let url = "https://www.reddit.com/user/" <> Text.unpack user
        alt = "View User"
        label = "/u/" <> str (Text.unpack user)
    in link url alt label
