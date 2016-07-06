{-# LANGUAGE OverloadedStrings #-}

module DailyProgrammer.Offline
    ( offline
    , writeOffline
    , writeFormats
    ) where

import           Control.Monad.IO.Class
import           DailyProgrammer.Posts
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BS
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Reddit
import           Reddit.Pandoc
import           Reddit.Types.Listing
import           Reddit.Types.Post
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.PDF
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.LaTeX

-- hasTable :: Monad m => Pipe Pandoc Pandoc m ()
-- hasTable = Pipes.filter (getAny . query (Any . isTable))
--   where
--     isTable Table{} = True
--     isTable _       = False
--
-- hasLink :: Monad m => Pipe Post Post m ()
-- hasLink = Pipes.filter (isLink . content)
--   where
--     isLink Reddit.Link{} = True
--     isLink _             = False

writeOffline :: Reddit ()
writeOffline = do
    result <- offline
    liftIO $ writeFormats "offline" result

offline :: MonadIO m => RedditT m Pandoc
offline = do
    now <- liftIO getCurrentTime
    dailyprogrammer <- getSubredditInfo (R "dailyprogrammer")
    posts <- foldMap fromPost <$> latest
    return
        . setTitle (fromSubreddit dailyprogrammer <> " Offline")
        . setAuthors [fromUsername (Username "wizao")]
        . setDate (fromUTCTime "%a %b %e, %Y %l:%M%p" now)
        $ posts

writeFormats :: FilePath -> Pandoc -> IO ()
writeFormats file document = do
    template <- readFile "template.latex"

    let options = redditMarkdownWriter
            { writerStandalone = True
            , writerTemplate = template
            , writerTableOfContents = True
            , writerTOCDepth = 2
            , writerEpubChapterLevel = 2 }

    docxBytes <- liftIO $ writeDocx options document
    BS.writeFile (file <> ".docx") docxBytes

    let texString = writeLaTeX options document
    writeFile (file <> ".tex") texString

    Right pdfBytes <- makePDF "pdflatex" writeLaTeX options document
    BS.writeFile (file <> ".pdf") pdfBytes
