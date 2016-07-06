{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad
import           DailyProgrammer.Offline
import           DailyProgrammer.WikiChallenges
import           Reddit

m :: IO ()
m = void $ runReddit "user" "password" updateChallengesWiki
-- m = void $ runRedditAnon writeOffline
