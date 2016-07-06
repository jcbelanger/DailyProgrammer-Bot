{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Reddit.Pandoc.Post
    ( fromPost
    , fromPostContent
    , postTitleLink
    ) where

import           Data.Foldable
import           Data.List
import qualified Data.Text                    as Text
import           Reddit
import           Reddit.Pandoc.RedditMarkdown
import           Reddit.Pandoc.User
import           Reddit.Pandoc.UTCTime
import           Reddit.Types.Post
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Walk

postTitleLink :: Post -> Inlines
postTitleLink Post{..} =
    let url = "https://www.reddit.com" <> Text.unpack permalink
        alt = "View Comments"
        label = text (Text.unpack title)
    in link url alt label

fromPost :: Post -> Pandoc
fromPost post@Post{..} =
    let date = fromUTCTime "%a %b %e, %Y %l:%M%p" created
    in doc $
        header 1 (postTitleLink post) <>
        para ("Submitted " <> date <> " by " <> fromUsername author) <>
        walk incHeader (fromPostContent content)

incHeader :: Block -> Block
incHeader (Header n attr xs) = Header (n+1) attr xs
incHeader x                  = x

fromPostContent :: PostContent -> Blocks
fromPostContent TitleOnly = para mempty
fromPostContent (Reddit.Link x) =
    let url = Text.unpack x
        alt = "View Online"
        text = str (Text.unpack x)
    in para (link url alt text)
fromPostContent (SelfPost markdown html)
    | Right (Pandoc _ body) <- readMarkdown redditMarkdownReader (Text.unpack markdown) = fromList body
    | Right (Pandoc _ body) <- readHtml     def                  (Text.unpack html)     = fromList body
    | otherwise =
        para (emph "Error! Could not parse post.") <>
        header 1 "Raw Content:" <>
        (plain . str . Text.unpack) markdown
