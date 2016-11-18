{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db ( allFeeds
          , createFeed
          , deleteFeed
          , feedsToUpdate
          , initDb
          , listFeeds
          , runSqliteLogged
          , setPause
          , showFeed
          , tweetables
          , updateFeed
          , wasTweeted
          , Feed (..)
          ) where

import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Logger -- (runLoggingT, LoggingT, runNoLoggingT, NoLoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Int (Int64)
import Data.Time
import Data.Time.Format
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.IO
import System.Log.FastLogger -- (ToLogStr (toLogStr), LogStr (..))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Config
import FetchFeed

type SqlPersistML = ReaderT SqlBackend (LoggingT (ResourceT IO))

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
    firstRun Bool
    paused Bool
    UniqueURI uri
    deriving Show
Entry
    feedId FeedId
    title T.Text
    link T.Text
    retrieved UTCTime
    tweeted Bool
    skip Bool
    UniqueLink link
    deriving Show
|]

initDb :: IO ()
initDb = do
    dbname <- dbName
    runSqliteLogged dbname $ do
      _ <- doMigrations
      liftIO $ putStrLn "Initialized database"

allFeeds :: SqlPersistML [Entity Feed]
allFeeds =
    selectList [] []

showFeed :: String -> IO ()
showFeed id = do
    dbname <- dbName
    runSqliteLogged dbname $ do
      feed <- get (toSqlKey (read id) :: Key Feed) :: SqlPersistML (Maybe Feed)
      case feed of
        Nothing -> liftIO . putStrLn $ "Can't show feed with id: " ++ show id
        Just f  -> liftIO $ display f
  where
    display f = do
      hSetBuffering stdout NoBuffering
      putStrLn $ "\nFeed id: " ++ show id
      putStrLn "------------"
      putStrLn $ "Feed URI   : " ++ T.unpack (feedUri f)
      putStrLn $ "Title      : " ++ T.unpack (feedTitle f)
      putStrLn $ "Prepend    : " ++ T.unpack (feedPrepend f)
      putStrLn $ "Append     : " ++ T.unpack (feedAppend f)
      putStrLn $ "Tweets/run : " ++ show (feedTweetsPerRun f)
      putStrLn $ "Check every: " ++ show (feedCheckEvery f) ++ " minutes"
      putStrLn $ "Next check : " ++ show (feedNextCheck f) ++ " (UTC)"
      putStrLn $ "First run? : " ++ show (feedFirstRun f)
      putStrLn $ "Paused?    : " ++ show (feedPaused f)
      putStrLn ""

listFeeds :: IO ()
listFeeds = do
    dbname <- dbName
    runSqliteLogged dbname $ do
      fs <- allFeeds
      if null fs
      then liftIO $ putStrLn "No feeds yet use 'rsstwit add' to create one."
      else mapM_ listFeed fs
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

-- Finds the feeds that need updating
feedsToUpdate :: SqlPersistML [Entity Feed]
feedsToUpdate = do
    now <- liftIO getCurrentTime
    selectList [FeedNextCheck <=. now, FeedPaused ==. False] []

createFeed :: Feed -> IO (Key Feed)
createFeed f = do
    dbname <- dbName
    runSqliteLogged dbname (insert f :: SqlPersistML (Key Feed))

deleteFeed :: Key Feed -> IO ()
deleteFeed fid = do
    dbname <- dbName
    runSqliteLogged dbname $ do
      delete fid::SqlPersistML ()
      deleteWhere [EntryFeedId  ==. fid ] :: SqlPersistML ()
      return ()

setPause :: Key Feed -> Bool -> IO ()
setPause fk b = do
    dbname <- dbName
    runSqliteLogged dbname $ do
      _ <- update fk [ FeedPaused =. b] :: SqlPersistML ()
      return ()

-- Given a feed entity, fetch its entries, fetch the feed,
-- compare the first maxEntries feed entries with the db entries
-- adding any unseen ones to the database and update next fetch time
-- If this is our first run mark all items skippable excepr the first
-- tweets per run tweets -- TODO check ordering!
-- Assumptions: The feed is in reverse chronological order so the tweets we
--              want to tweet are at the "top".
--              Links are used for comparison rather than ids which are often
--              absent from feeds in the wild. But it will fuck up if the same 
--              link is in the feed more than once.
updateFeed :: Entity Feed -> SqlPersistML [Key Entry]
updateFeed f = do
    postables <- liftIO $ fetchFeed (feedUri (entityVal f))
    case postables of
      Nothing -> return [] -- failed to fetch feed
      Just ps -> do
        maxEs <- liftIO maxEntries
        let ps' = take maxEs ps
            fk  = entityKey f
            fv  = entityVal f
            ffr = feedFirstRun fv
            -- If this is not the first run, we only want tweetsPerRun entries
            ps'' = if ffr then ps' else take (feedTweetsPerRun fv) ps'

        -- We want the postables that are *not* in the database, get the ones that are in it
        -- And the ps not in that list are the ones not in the db...
        inDbs <- seenLinks ps' fk
        let toAdds  = filter (\p -> link p `notElem` inDbs) ps''

        now <- liftIO getCurrentTime
        _ <- update fk [ FeedNextCheck =. addUTCTime (realToFrac $ feedCheckEvery fv) now
                       , FeedFirstRun =. False]
        es <- mapM (createEntry f) toAdds
        let es' = if ffr then take (feedTweetsPerRun fv) es else es
        mapM_ (\k -> update k [ EntrySkip =. False]) es' 
        return es'

-- Given a feed and a postable, insert a new Entry, with 0 tweeted, feed id set
-- the link, title and pubDate from the postable, retrieved set to now
createEntry :: Entity Feed -> Postable -> SqlPersistML (Key Entry)
createEntry f p = do
    now <- liftIO getCurrentTime
    insert $ Entry (entityKey f) -- entryFeedId
                   (title p)     -- entryTitle
                   (link p)      -- entryLink
                   now           -- entryRetrieved
                   False         -- entryTweeted
                   True          -- entrySkip

wasTweeted :: Key Entry -> SqlPersistML ()
wasTweeted ek =
    update ek [EntryTweeted =. True]

seenLinks :: [Postable] -> Key Feed -> SqlPersistML [T.Text]
seenLinks ls fk = do
      xs <- selectList
                 [ EntryFeedId ==. fk
                 , EntryLink <-. map link ls
                 ] [ ]
      return $ map (entryLink . entityVal) xs

-- Given a feed, find tweetable entries and return formatted tweet texts
tweetables :: Entity Feed -> SqlPersistML [(Key Entry,T.Text)]
tweetables f = do
    ul <- liftIO shortUrlLength
    let fv   = entityVal f
        fk   = entityKey f
        pre  = feedPrepend fv
        preS = if T.null pre then T.empty else pre +++ " "
        post = feedAppend fv
        sPost= if T.null post then T.empty else " " +++ post
        tl   = 140 - ul - (T.length preS + T.length sPost)
    es <- selectList [ EntryFeedId  ==. fk
                     , EntryTweeted ==. False
                     , EntrySkip    ==. False
                     ]
                     [ Desc EntryRetrieved
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

--doMigrations :: SqlPersistML ()
doMigrations :: SqlPersistML ()
doMigrations =
    runMigration migrateAll

-- Log sql statements to file as we go along
runSqliteLogged :: (MonadBaseControl IO m, MonadIO m)
          => T.Text                                 -- ^ connection string
          -> SqlPersistT (LoggingT (ResourceT m)) a -- ^ database action
          -> m a
runSqliteLogged connstr action = do
    lp <- liftIO logPath
    logFile <- liftIO $ openFile (T.unpack lp) AppendMode
    ma <- runResourceT
         . (`runLoggingT` formatOutput logFile)
         . withSqliteConn connstr
         $ runSqlConn action
    liftIO $ hClose logFile
    return ma

-- this is pretty much just borrowed from the impl of defaultOutput 
-- in Control.Monad.Logger
formatOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
formatOutput h loc src level msg =
    S8.hPutStrLn h $ S8.concat bs
  where
    bs =
        [ S8.pack "["
        , case level of
            LevelOther t -> encodeUtf8 t
            _ -> encodeUtf8 $ T.pack $ drop 5 $ show level
        , if T.null src
            then S8.empty
            else encodeUtf8 $ '#' `T.cons` src
        , S8.pack "] "
        , fromLogStr msg
        , S8.pack "\n"
        ]

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
         False
         False
