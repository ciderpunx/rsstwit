{-# LANGUAGE OverloadedStrings #-}

module FetchFeed ( fetchFeed
                 , Postable (..)
                 ) where

import Control.Exception as X
import Network.HTTP.Conduit
import Text.XML
import Text.XML.Cursor
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

-- Only title and link are used in the database.
data Postable = Postable { title :: T.Text
                         , link :: T.Text
                         , description :: T.Text
                         , pubDate :: T.Text
                         } deriving (Show)

type URI = T.Text

fetchFeed :: URI -> IO (Maybe [Postable])
fetchFeed uri = do
    let u = T.unpack $ T.strip uri
    src <- simpleHttp u `X.catch` fetchHandler
    if src == L.empty
    then return Nothing
    else do
      let doc    = parseLBS_ def src
          cursor = fromDocument doc
          rss    =  cursor $.// laxElement "rss"
      return . Just $
            if null rss -- we got a feed that doesn't look like an rss
            then map atomToPostable (cursor $// laxElement "entry")
            else map rssToPostable (cursor $// laxElement "item")

fetchHandler :: HttpException -> IO L.ByteString
fetchHandler e =
    putStrLn "Error occurred during download" >> print e >> return L.empty

atomToPostable :: Cursor -> Postable
atomToPostable entry =
    let
      link    = head $ entry $/ laxElement "link" >=> laxAttribute "href"
      desc    = head $ entry $/ laxElement "summary" &/ content
      title   = head $ entry $/ laxElement "title" &/ content
      pubdate = head $ entry $/ laxElement "published" &/ content
    in
      Postable { title = title
               , description = desc
               , pubDate = pubdate
               , link = link
               }

rssToPostable :: Cursor -> Postable
rssToPostable item =
    let
      link    = head $ item $/ laxElement "link" &/ content
      desc    = head $ item $/ laxElement "description" &/ content
      title   = head $ item $/ laxElement "title" &/ content
      pubdate = head $ item $/ laxElement "pubDate" &/ content
    in
      Postable { title = title
               , description = desc
               , pubDate = pubdate
               , link = link
               }
