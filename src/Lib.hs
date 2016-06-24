{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad
import           DailyProgrammer.Offline
import           Reddit

m :: IO ()
m = void . runRedditAnon $ offlineDocx "offline.docx"
