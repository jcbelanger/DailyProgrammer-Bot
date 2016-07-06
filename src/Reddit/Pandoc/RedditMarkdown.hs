
module Reddit.Pandoc.RedditMarkdown
    ( redditMarkdownExtensions
    , redditMarkdownReader
    , redditMarkdownWriter
    ) where

import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Text.Pandoc

redditMarkdownExtensions :: Set Extension
redditMarkdownExtensions = Set.fromList
    [ Ext_pipe_tables
    , Ext_backtick_code_blocks
    , Ext_strikeout
    , Ext_superscript
    , Ext_all_symbols_escapable
    ]

redditMarkdownReader :: ReaderOptions
redditMarkdownReader = def
    { readerExtensions = redditMarkdownExtensions
    }

redditMarkdownWriter :: WriterOptions
redditMarkdownWriter = def
    { writerExtensions = redditMarkdownExtensions
    , writerWrapText = WrapNone
    }
