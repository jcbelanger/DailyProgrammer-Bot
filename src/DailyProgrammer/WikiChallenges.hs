{-# LANGUAGE OverloadedStrings #-}

module DailyProgrammer.WikiChallenges
    ( updateChallengesWiki
    , simpleRedditTable
    ) where

import           DailyProgrammer.Posts
import           Data.Default.Class
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Reddit
import           Reddit.Pandoc
import           Reddit.Types.Post
import           Reddit.Types.Wiki
import           Text.Pandoc
import           Text.Pandoc.Builder

updateChallengesWiki :: Monad m => RedditT m ()
updateChallengesWiki = do
    table <- toTable <$> latest
    let wikiDoc =  doc table --setTitle "Past Challenges" (doc table)
        wikiUrl = "challenges"
        editReason = "DailyProgrammer-Bot updating the latest challenges"
        wikiContent = Text.pack (writeMarkdown redditMarkdownWriter wikiDoc)
    editWikiPage (R "dailyprogrammerbot") wikiUrl wikiContent editReason

toTable :: [Post] -> Blocks
toTable = simpleRedditTable ["Easy", "Intermediate", "Hard", "Other"] . toRows

toRows :: [Post] -> [[Inlines]]
toRows = fmap toRow .  nonOverlap

--weekly grouping doesn't work for earliest posts. Using nonOverlap instead
weekly :: [Post] -> [[Post]]
weekly = groupBy ((==) `on` week . postDay)

week :: Day -> (Integer, Int)
week day = let (year, weekNum, _) = toWeekDate day in (year, weekNum)

nonOverlap :: [Post] -> [[Post]]
nonOverlap = fmap Map.elems . uncurry (:) . foldr go (Map.empty,[])
  where
    go post (x,xs)
        | Map.member diff x = (Map.singleton diff post, x:xs)
        | otherwise         = (Map.insert diff post x ,   xs)
        where diff = postDifficulty post

toRow :: [Post] -> [Inlines]
toRow row = [toCell (Map.lookup col diffMap) | col <- [Just Easy, Just Intermediate, Just Hard, Nothing]]
  where
    diffMap = Map.fromList [(postDifficulty post, post) | post <- row]

toCell :: Maybe Post -> Inlines
toCell = maybe (emph "-") postTitleLink

-- toCell' :: [Post] -> Inlines
-- toCell' []    = emph "-"
-- toCell' posts = (fold . intersperse ", " . map postTitleLink . sortOn created) posts

--Pandoc's pipe table rendering causes too many issues for reddit (output size)
--However, Pandoc is still useful for escaping markdown text in table cells
simpleRedditTable :: [Inlines] -> [[Inlines]] -> Blocks
simpleRedditTable headers rows =
    let mdPipes = plain . fold . intersperse (rawInline "markdown" "|")
        aligns = rawInline "markdown" ":--" <$ headers
    in mdPipes headers <> mdPipes aligns <> foldMap mdPipes rows
