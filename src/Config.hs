{-# LANGUAGE OverloadedStrings #-}

module Config (maxEntries, shortUrlLength, dbName, getTwinfo) where

import Prelude hiding (lookup)
import Data.Configurator
import Data.Configurator.Types
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath)
import qualified Control.Exception as X
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Maybe (isNothing)
import Web.Twitter.Conduit hiding (lookup)

configFileName = "rsstwit.cfg"
dbFilename     = "rsstwit.sqlite"
dbFallbackFile = "/tmp/rsstwit.sqlite3"

-- TODO catch isDoesNotExist error on getAppUserDataDirectory
config :: IO Config
config = do
    confdir <- getAppUserDataDirectory "rsstwit"
    createDirectoryIfMissing True confdir
    let conffile = joinPath [confdir, configFileName]
    exists <- doesFileExist conffile
    if exists
    then load [Required conffile]
    else do
      putStrLn $ "Error: Couldn't read config file.\nWill create a default file at: "
                 ++ show conffile
      putStrLn "ACTION: You will need to fill in yout Twitter credentials in it.\n"
      writeFile conffile defaultConfigString
      load [Required conffile]

-- Name for the database, which is stored in the user data directory. 
-- If not present fall back to dbFallbackFile.
dbName :: IO String
dbName = do
    c <- config
    confdir <- getAppUserDataDirectory "rsstwit"
    let dbDef = joinPath [confdir, dbFilename]
    dn <- lookupDefault dbFallbackFile c (T.pack "dbName")
    return $ if dn == dbFallbackFile then dn else joinPath [confdir, dn]

-- Maximum allowed number of entries in a feed
maxEntries :: IO Int
maxEntries = do
    c <- config
    lookupDefault 10 c (T.pack "maxEntries")

-- Maximum length of a t.co link (used in calculating amount of text to tweet)
shortUrlLength :: IO Int
shortUrlLength = do
    c <- config
    lookupDefault 24 c (T.pack "shortUrlLength")

-- Returns credentials needed for us to be able to tweet as a TWInfo
getTwinfo :: IO TWInfo
getTwinfo = do
    tokens <- twitterConsumerTokens
    credential <- twitterOauthCredentials
    return $ setCredential tokens credential def

-- Fetch oauth consumer key and secret from config, error if not found
twitterConsumerTokens :: IO OAuth
twitterConsumerTokens = do
    c <- config
    k <- lookup c (T.pack "twitterConsumerKey")
    s <- lookup c (T.pack "twitterConsumerSecret")
    case (k,s) of
      (Nothing,_) -> error
                      "Couldn't read twitter oauth consumer key from config file"
      (_,Nothing) -> error
                      "Couldn't read twitter oauth consumer secret from config file"
      (Just key, Just sec) -> return $ twitterOAuth
            { oauthConsumerKey    = key
            , oauthConsumerSecret = sec
            }

-- Fetch oauth access token and secret from config, error if not found
twitterOauthCredentials :: IO Credential
twitterOauthCredentials = do
    c <- config
    t <- lookup c (T.pack "twitterOauthToken")
    s <- lookup c (T.pack "twitterOauthSecret")
    case (t,s) of
      (Nothing,_) -> error
          "Couldn't read twitter oauth access token from config file"
      (_,Nothing) -> error
          "Couldn't read twitter oauth access secret from config file"
      (Just tok, Just sec) -> return $ Credential
            [ ("oauth_token", tok)
            , ("oauth_token_secret", sec)
            ]

-- If no config is found, this default string will be written to the file
defaultConfigString :: String
defaultConfigString =
    unlines [ "# rsstwit configuration file"
            , ""
            , "# Name for your database"
            , "dbName = \"rsstwit.sqlite\""
            , ""
            , "# Max number of feed entries to fetch when the cronjob runs"
            , "maxEntries = 10"
            , ""
            , "# Max length in characters of a t.co link + 1 cf: https://dev.twitter.com/rest/reference/get/help/configuration"
            , "shortUrlLength = 24"
            , ""
            , "# Twitter API info; you will need to supply these before rsstwit works"
            , "# twitterConsumerKey    = \"\""
            , "# twitterConsumerSecret = \"\""
            , "# twitterOauthToken     = \"\""
            , "# twitterOauthSecret    = \"\""
            ]
