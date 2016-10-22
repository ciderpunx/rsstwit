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

dbname = ":memory:" -- "app/rsstwit.sqlite3"

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
    retrieved UTCTime
    tweeted Bool
    deriving Show
|]

doMigrations :: SqlPersistM ()
doMigrations =
    runMigration migrateAll

feedsToUpdate :: SqlPersistM [Entity Feed]
feedsToUpdate = do
    now <- liftIO getCurrentTime
    selectList [FeedNextCheck <=. now] []

createFeed :: Feed -> IO (Key Feed)
createFeed f = runSqlite dbname $ do 
    doMigrations
    insert f

-- Given a feed entity, fetch its entries, fetch the feed, compare the first maxEntries feed entries with
-- the db entries adding any unseen ones to the database
-- Assumptions: The feed is in reverse chronological order.
--              Links are used for comparison rather than ids which are often
--              absent from feeds in the wild.
updateFeed f = do
    dbentries <- entriesForFeed f
    let dblinks = map (entryLink . entityVal) dbentries
    postables <- liftIO $ fetchFeed (feedUri (entityVal f))
    let toAdds = filter (\p -> link p `notElem` dblinks) postables
        fk = entityKey f
        fv = entityVal f
    _ <- update fk [FeedNextCheck =. addUTCTime (realToFrac $ feedCheckEvery fv) (feedNextCheck fv)]
    return $ if null toAdds
             then []
             else map (createEntry f) toAdds

-- Given a feed and a postable, insert a new Entry, with 0 tweeted, feed id set
-- the link, title and pubDate from the postable, retrieved set to now
createEntry :: Entity Feed -> Postable -> SqlPersistM (Key Entry)
createEntry f p = do
    now <- liftIO getCurrentTime
    insert $ Entry (entityKey f) -- entryFeedId
                   (title p)     -- entryTitle
                   (link p)      -- entryLink
                   now           -- entryRetrieved
                   False         -- entryTweeted

entriesForFeed :: Entity Feed -> SqlPersistM [Entity Entry]
entriesForFeed f =
    selectList [ EntryFeedId ==. entityKey f
               , EntryTweeted ==. False
               ]
               [ Asc EntryRetrieved
               ]

tweetableEntries :: Entity Feed -> SqlPersistM [Entity Entry]
tweetableEntries f =
    selectList [ EntryFeedId ==. entityKey f
               , EntryTweeted ==. False
               ]
               [ Asc EntryRetrieved
               , LimitTo (feedTweetsPerRun (entityVal f))
               ]

-- This should be somewhere else!
main :: IO ()
main = runSqlite dbname $ do
    doMigrations
    fid <- insert egFeed
    feeds <- feedsToUpdate
    liftIO $ print feeds

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

