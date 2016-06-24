{-# LANGUAGE OverloadedStrings #-}

module DailyProgrammer.Posts
    ( dailyProgrammer
    ) where

import           Data.Default.Class
import           Pipes
import           Pipes.Prelude        as Pipes
import           Reddit
import           Reddit.Pipes.Post
import           Reddit.Types.Post
import           Reddit.Types.Listing

dailyProgrammer :: Monad m => Producer Post (RedditT m) ()
dailyProgrammer = Pipes.concat <-< getPostsAfterP def New (Just (R "dailyprogrammer"))
