{-# LANGUAGE OverloadedStrings #-}

module FetchFeed ( fetchFeed
                 , Postable (..)
                 ) where

import Control.Exception as X
import Data.Maybe (fromJust, catMaybes, isNothing)
import Network.HTTP.Conduit
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

-- Only title and link are used in the database.
data Postable = Postable { title :: T.Text
                         , link :: T.Text
                         , pubDate :: T.Text
                         } deriving (Show)

type URI = T.Text

fetchFeed :: URI -> IO (Maybe [Postable])
fetchFeed uri = do
    let u = T.unpack $ T.strip uri
    -- print $ "Fetching: " ++ u
    src <- simpleHttp u `X.catch` fetchHandler
    if src == L.empty
    then return Nothing
    else case parseLBS def src of
        Left err  -> error $ "Couldn't parse " ++ u ++ "\n\t" ++ show err
        Right doc -> getElts doc `X.catch` eltHandler u

getElts :: Document -> IO (Maybe [Postable])
getElts doc = do
    let cursor = fromDocument doc
        rss    = cursor $.// laxElement "rss"
        isAtom = null rss
    ps <- extractPostables isAtom cursor `X.catch` postHandler
    let qs = map fromJust ps
    return $ Just qs

extractPostables :: Bool -> Cursor -> IO [Maybe Postable]
extractPostables isAtom cursor = return $
    if  isAtom
    then map atomToPostable (cursor $// laxElement "entry")
    else map rssToPostable (cursor $// laxElement "item")

postHandler :: X.SomeException -> IO [Maybe Postable]
postHandler e =
    putStrLn "Error building postable from feed\n\t" >> print e >> return [Nothing]

eltHandler :: String -> X.SomeException -> IO (Maybe [Postable])
eltHandler u e =
    putStr "Error extracting elements from feed: " >> putStr u >>  putStr "\n\t" >> print e >> return Nothing

fetchHandler :: HttpException -> IO L.ByteString
fetchHandler e =
    putStrLn "Error occurred during download\n\t" >> print e >> return L.empty

atomToPostable :: Cursor -> Maybe Postable
atomToPostable entry =
    let
      link    = sHead $ entry $/ laxElement "link" >=> laxAttribute "href"
      title   = sHead $ entry $/ laxElement "title" &/ content
      pubdate = sHead $ entry $/ laxElement "published" &/ content
    in
      case (link, title, pubdate) of
        (Just l, Just t, Just p) ->
            Just Postable { title   = t
                          , pubDate = p
                          , link    = l
                          }
        _ -> Nothing

rssToPostable :: Cursor -> Maybe Postable
rssToPostable item =
    let
      link    = sHead $ item $/ laxElement "link" &/ content
      title   = sHead $ item $/ laxElement "title" &/ content
      pubdate = sHead $ item $/ laxElement "pubDate" &/ content
      dcdate  = sHead $ item $/ laxElement "date" &/ content -- a list apart uses dc:date instead of pubDate
    in
      case (link, title, pubdate, dcdate) of
        (Just l, Just t, Just p, _) ->
            Just Postable { link    = l
                          , title   = t
                          , pubDate = p
                          }
        (Just l, Just t, _, Just d) ->
            Just Postable { link    = l
                          , title   = t
                          , pubDate = d
                          }
        _ -> Nothing

sHead :: [T.Text] -> Maybe T.Text
sHead [] = Nothing
sHead xs = Just $ head xs
