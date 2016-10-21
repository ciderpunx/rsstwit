{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Time
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkSave "entityDefs"] [persistLowerCase|
Feed
    uri T.Text
    title T.Text
    prepend T.Text
    append T.Text
    tweetsPerRun Int
    checkEvery Int
    nextCheck UTCTime
    deriving Show
Entry
    feedId FeedId
    title T.Text
    link T.Text
    pubDate UTCTime
    retrieved UTCTime
    tweeted Bool
    deriving Show
|]

feedsToUpdate = do
    now <- liftIO getCurrentTime
    fs <- selectList [FeedNextCheck <=. now] []
    return $ map entityVal fs

doMigrations =
    runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Feed)

-- createFeed (Feed ...) - create new feed, Should be able to touch from CLI

-- updateFeed
--   -- get entries from db
--   -- if more than 0
--   --    -- They need tweeting (Twitter.tweetEntry)
--   -- otherwise
--   --    -- Get the feed from the internet (FetchFeed.fetchFeed)
--   --    -- add any new entries (insertEntries)
--   --    -- tweet feed.tweetsPerRun entries (and mark as tweeted)

-- updateFeeds

-- createEntries  = map createEntry

-- createEntry (Entry ...) - create new entry

-- This should be somewhere else!
main :: IO ()
main = runSqlite ":memory:" $ do
    doMigrations
    fid <- insert $ Feed "http://charlieharvey.org.uk/page/feed/rss" 
                         "Charlie RSS"
                         ""
                         " #charlieharvey"
                         1
                         30
                         (UTCTime (fromGregorian 2016 10 20) 1200)
    feeds <- feedsToUpdate
    liftIO $ print feeds
