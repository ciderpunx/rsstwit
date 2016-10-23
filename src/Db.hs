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

import Control.Monad.Reader (ReaderT)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource.Internal (ResourceT)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Time.Format
import qualified Data.Text as T
import FetchFeed


-- TODO: deleteFeed
-- todo external config file for these!
dbname = "test/mock.sqlite3"
         -- ":memory:"
         -- "app/rsstwit.sqlite3"
maxEntries = 10
shortUrlLength = 24 -- short url plus space!

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      , mkSave "entityDefs"
      ]
      [persistLowerCase|
Feed
    uri T.Text
    title T.Text
    prepend T.Text
    append T.Text
    tweetsPerRun Int
    checkEvery Int
    nextCheck UTCTime
    UniqueURI uri
    deriving Show
Entry
    feedId FeedId
    title T.Text
    link T.Text
    retrieved UTCTime
    tweeted Bool
    deriving Show
|]

allFeeds :: SqlPersistM [Entity Feed]
allFeeds =
    selectList [] []

-- maybe todo: convert t all use T.text
listFeeds :: IO ()
listFeeds = runSqlite dbname $ do
    fs <- allFeeds
    mapM_ listFeed fs
  where
    listFeed f = do
      let fv = entityVal f
          fk = entityKey f
      liftIO . putStrLn $ show (fromSqlKey fk)
                       ++ ": "
                       ++ (take 25 . T.unpack $ feedTitle fv)
                       ++ " ("
                       ++ (take 50 . T.unpack $ feedUri fv)
                       ++ ") Next check: "
                       ++ show (feedNextCheck fv)

-- Finds the feeds that need updadting
feedsToUpdate :: SqlPersistM [Entity Feed]
feedsToUpdate = do
    now <- liftIO getCurrentTime
    selectList [FeedNextCheck <=. now] []

createFeed :: Feed -> IO (Key Feed)
createFeed f =
    runSqlite dbname (insert f :: SqlPersistM (Key Feed))

deleteFeed :: Key Feed -> IO ()
deleteFeed fid = runSqlite dbname $ do
    delete fid::SqlPersistM ()
    deleteWhere [EntryFeedId  ==. fid ] :: SqlPersistM ()
    return ()

-- Given a feed entity, fetch its entries, fetch the feed,
-- compare the first maxEntries feed entries with the db entries
-- adding any unseen ones to the database and update next fetch time
-- Assumptions: The feed is in reverse chronological order.
--              Links are used for comparison rather than ids which are often
--              absent from feeds in the wild.
updateFeed :: Entity Feed -> SqlPersistM [Key Entry]
updateFeed f = do
    dbentries <- entriesForFeed f
    postables <- liftIO $ fetchFeed (feedUri (entityVal f))
    case postables of
      Nothing -> return [] -- failed to fetch feed
      Just ps -> do
        let dblinks = map (entryLink . entityVal) dbentries
            toAdds  = filter (\p -> link p `notElem` dblinks) (take maxEntries ps)
            fk      = entityKey f
            fv      = entityVal f
        now <- liftIO getCurrentTime
        _ <- update fk [FeedNextCheck =. addUTCTime (realToFrac $ feedCheckEvery fv) now]
        mapM (createEntry f) toAdds

-- Given a feed and a postable, insert a new Entry, with 0 tweeted, feed id set
-- the link, title and pubDate from the postable, retrieved set to now
createEntry f p = do
    now <- liftIO getCurrentTime
    insert $ Entry (entityKey f) -- entryFeedId
                   (title p)     -- entryTitle
                   (link p)      -- entryLink
                   now           -- entryRetrieved
                   False         -- entryTweeted

wasTweeted ek = update ek [EntryTweeted =. True]

entriesForFeed :: Entity Feed -> SqlPersistM [Entity Entry]
entriesForFeed f =
    selectList [ EntryFeedId ==. entityKey f
               , EntryTweeted ==. False
               ]
               [ Asc EntryRetrieved
               ]

-- Given a feed, find tweetable entries and return formatted tweet texts
tweetables :: Entity Feed -> SqlPersistM [(Key Entry,T.Text)]
tweetables f = do
    let fv   = entityVal f
        fk   = entityKey f
        pre  = feedPrepend fv
        preS = if T.null pre then T.empty else pre +++ " "
        post = feedAppend fv
        sPost= if T.null post then T.empty else " " +++ post
        tl   = 140 - shortUrlLength - (T.length preS + T.length sPost)
    es <- selectList [ EntryFeedId ==. fk
                     , EntryTweeted ==. False
                     ]
                     [ Asc EntryRetrieved
                     , LimitTo (feedTweetsPerRun fv)
                     ]
    return $
      map (\e -> ( entityKey e
                 , preS +++ entryLink (entityVal e)
                        +++ " "
                        +++ T.take tl (entryTitle $ entityVal e)
                        +++ sPost
                 )
          ) es
  where
    (+++) = T.append

doMigrations :: SqlPersistM ()
doMigrations =
    runMigration migrateAll

-- can be used for testing
egFeed :: Feed
egFeed =
    Feed "http://charlieharvey.org.uk/page/feed/rss" 
         "Charlie's RSS"
         ""
         " #charlieharvey"
         1
         30
         (UTCTime (fromGregorian 2016 10 20) 1200)
