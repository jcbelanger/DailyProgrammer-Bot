{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DailyProgrammer.Offline
    ( offline
    , offlineDocx
    , offlinePDF
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           DailyProgrammer.Posts
import qualified Data.ByteString.Lazy      as BS
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Data.Time.Format
import           Pipes
import qualified Pipes.Prelude             as Pipes
import           Reddit
import           Reddit.Types.Listing
import           Reddit.Types.Post
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.PDF
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.LaTeX

redditMarkDown :: ReaderOptions
redditMarkDown = def { readerExtensions = pandocExtensions }

fromPost :: Post -> Pandoc
fromPost Post{..} =
    let url = "https://www.reddit.com" <> T.unpack permalink
        label = str (T.unpack title)
        top = doc $
            header 1 (link url "View Comments" label) <>
            para ("Submitted " <> fromTime created <> " by " <> fromUsername author)
    in top <> walk incHeader (fromContent content)

incHeader :: Block -> Block
incHeader (Header n attr xs) = Header (n+1) attr (walk incHeader xs)
incHeader x                  = x

fromTime :: UTCTime -> Inlines
fromTime = str . formatTime defaultTimeLocale "%a %b %e, %Y %l:%M%p"

fromContent :: PostContent -> Pandoc
fromContent TitleOnly = mempty
fromContent (Reddit.Link url) = doc $ para $ link (T.unpack url) "View Online" (str $ T.unpack url)
fromContent (SelfPost markdown html)
    | Right result <- readMarkdown redditMarkDown (T.unpack markdown) = result
    | Right result <- readHtml     def            (T.unpack html)     = result
    | otherwise = doc $
        para "Error! Could not parse post." <>
        header 2 "Raw Content:" <>
        plain (str $ T.unpack markdown)

fromUsername :: Username -> Inlines
fromUsername (Username user) =
    let strUser = T.unpack user
        url = "https://www.reddit.com/user/" ++ strUser
        title = "View User"
        label = "/u/" <> str strUser
    in link url title label

foldMapP :: (Monad m, Monoid b) => (a -> b) -> Producer a m () -> m b
foldMapP f p = Pipes.fold (<>) mempty id (Pipes.map f <-< p)

hasTable :: Monad m => Pipe Pandoc Pandoc m ()
hasTable = Pipes.filter (getAny . query (Any . isTable))
  where
    isTable Table{} = True
    isTable _       = False

hasLink :: Monad m => Pipe Post Post m ()
hasLink = Pipes.filter (isLink . content)
  where
    isLink Reddit.Link{} = True
    isLink _             = False

offline :: MonadIO m => RedditT m Pandoc
offline = do
    let subLink = link "https://reddit.com/r/dailyprogrammer/" "View Online" (str "/r/DailyProgrammer")
    now <- liftIO getCurrentTime
    posts <- foldMapP id (Pipes.take 4 <-< hasTable <-< Pipes.map fromPost <-< dailyProgrammer)
    return
        . setTitle (subLink <> " Offline")
        . setAuthors [fromUsername (Username "wizao")]
        . setDate (fromTime now)
        $ posts

offlineDocx :: FilePath -> Reddit ()
offlineDocx file = do
    result <- offline
    template <- liftIO $ readFile "template.latex"
    let options = def
            { writerStandalone = True
            , writerTemplate = template
            , writerTableOfContents = True }
    docxBytes <- liftIO $ writeDocx def result
    liftIO $ BS.writeFile file docxBytes

offlinePDF :: FilePath -> Reddit ()
offlinePDF file = do
    result <- offline
    template <- liftIO $ readFile "template.latex"
    let options = def
            { writerStandalone = True
            , writerTemplate = template
            , writerTableOfContents = True }
    liftIO $ writeFile "out.tex" $ writeLaTeX options result
    Right pdfBytes <- liftIO $ makePDF "pdflatex" writeLaTeX options result
    liftIO $ BS.writeFile file pdfBytes
