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

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|
Feed
    uri T.Text
    title T.Text
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

main :: IO ()
main = runSqlite ":memory:" $ do
    -- this line added: that's it!
    runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Feed)
    michaelId <- insert $ Feed "http://charlieharvey.org.uk/page/feed/rss" "Charlie RSS" 
                               1
                               30
                               (UTCTime (fromGregorian 2016 10 21) 1200)
    michael <- get michaelId
    liftIO $ print michael
