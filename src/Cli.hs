module Cli (main) where

-- Here we deal with command line interaction and running as a cron job
import Control.Monad (unless,join, filterM)
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Network.HTTP.Conduit (parseRequest, Request (..), HttpException (..) )
import Options.Applicative
import System.IO
import Text.Read (readMaybe)
import qualified Control.Exception as X
import qualified Data.Text as T

import Db
import FetchFeed
import Twitter
import Config

opts :: Parser (IO ())
opts = subparser
    ( command "delete" (info (delFeed <$> argument str idm) idm)
   <> command "add"    (info (pure addFeed) idm)
   <> command "list"   (info (pure listFeeds) idm)
   <> command "cron"   (info (pure cronRun) idm)
   <> command "init"   (info (pure initDb) idm)
    )

main :: IO ()
main = join $ execParser (info opts idm)

delFeed :: String -> IO ()
delFeed n =
    deleteFeed $ toSqlKey (read n :: Int64)

cronRun :: IO ()
cronRun = do
  dbname <- dbName
  runSqlite dbname $ do
    fsToUpdate <- feedsToUpdate
    mapM_ updateFeed fsToUpdate
    toTweets <- concatMapM tweetables fsToUpdate
    sentTweets <- filterM (liftIO . sendTweet) toTweets
    mapM_ (wasTweeted . fst) sentTweets
  where
    sendTweet t = do
        tweet <- tweetText (snd t)
        case tweet of
          Nothing -> return False
          Just _  -> return True

addFeed :: IO ()
addFeed = do
    hSetBuffering stdout NoBuffering
    uri          <- getUri "Feed URI: "
    title        <- getStrLen "Feed name (up to 100 chars): " 100
    prepend      <- getStrLen "Prepend to tweets (up to 10 chars): " 10
    append       <- getStrLen "Append to tweets (up to 20 chars - length of prepend): " (20 - length prepend)
    tweetsPerRun <- getIntLine "Tweets per run (1-5): " 1 5
    checkEvery   <- getIntLine "Check every n minutes (5 and above): " 5 9999999
    putStr "OK? (Y for yes anything else for no): "
    ok <- getLine
    now <- getCurrentTime
    if ok == "Y" || ok == "y"
      then do createFeed $
                Feed (T.pack uri)
                     (T.pack title)
                     (T.pack prepend)
                     (T.pack append)
                     tweetsPerRun
                     (checkEvery * 60)
                    now  -- will be checked on next cron run
              putStrLn "Feed created, will start tweeting when the next cron job runs."
              return ()
      else do putStr "Give up? (Q for quit anything else to try again): "
              q <- getLine
              unless (q == "Q" || q == "q") addFeed

getUri :: String -> IO String
getUri m = do 
    putStr m
    s <- getLine
    req <- fmap Just (parseRequest s) `X.catch` handleBadUri
    case req of
      Just _ -> return s
      Nothing -> getUri m
  where
    handleBadUri :: HttpException -> IO (Maybe Request)
    handleBadUri e = do
        putStrLn $ "Error: Can't parse URI\n\t" ++ show e
        return Nothing

getStrLen :: String -> Int -> IO String
getStrLen m max = do
    putStr m
    s <- getLine
    if length s > max
    then do putStrLn $ "Error: String should be maximum of " ++ show max ++ " characters"
            getStrLen m max
    else return s

getIntLine :: String -> Int -> Int -> IO Int
getIntLine m min max = do
    putStr m
    n <- getLine
    case readMaybe n of
      Just i -> if i >= min && i <= max
                then return i
                else do putStrLn $ "Error: Must be between " ++ show min ++ " and " ++ show max
                        getIntLine m min max
      Nothing -> getIntLine m min max
