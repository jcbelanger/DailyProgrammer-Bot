{-# LANGUAGE OverloadedStrings #-}

module DailyProgrammer.Posts
    ( Difficulty (..)
    , latest
    , latestP
    , postDifficulty
    , postDay
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Default.Class
import           Data.Foldable
import           Data.Function
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as Text
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Pipes
import qualified Pipes.Prelude                as Pipes
import           Reddit
import           Reddit.Pipes.Post
import           Reddit.Types.Listing
import           Reddit.Types.Post
import           Text.ParserCombinators.ReadP


data Difficulty = Easy | Intermediate | Hard deriving (Eq, Ord, Show, Enum, Bounded)

postDifficulty :: Post -> Maybe Difficulty
postDifficulty = runReadP (inBracketsP difficultyP) . Text.unpack . title

postDay :: Post -> Day
postDay post =
    let titleDay = runReadP (inBracketsP dayP) (Text.unpack $ title post)
        createdDay = utctDay (created post)
    in createdDay --fromMaybe createdDay titleDay

inBracketsP :: ReadP a -> ReadP a
inBracketsP p = munch (/='[') *> char '[' *> (inBrackets <|> nextBrackets)
    where
        inBrackets = p <* char ']'
        nextBrackets = munch (/=']') *> char ']' *> inBracketsP p

runReadP :: ReadP a -> String -> Maybe a
runReadP p = asum . map (pure . fst) . readP_to_S p

dayP :: ReadP Day
dayP = choice $ readPTime True defaultTimeLocale <$> ["%Y-%m-%d", "%m/%d/%Y", "%Y-%-m-%-d", "%-m/%-d/%Y"]

difficultyP :: ReadP Difficulty
difficultyP = choice
    [ Easy         <$ stringCI "Easy"
    , Intermediate <$ stringCI "Intermediate"
    , Intermediate <$ stringCI "Medium"
    , Hard         <$ stringCI "Hard"
    , Hard         <$ stringCI "Difficult"
    ]

stringCI :: String -> ReadP String
stringCI = traverse charCI

charCI :: Char -> ReadP Char
charCI c = char (toLower c) <|> char (toUpper c)

latestP :: Monad m => Producer Post (RedditT m) ()
latestP = Pipes.concat <-< getPostsAfterP def New (Just (R "dailyprogrammer"))

latest :: Monad m => RedditT m [Post]
latest = Pipes.toListM latestP
