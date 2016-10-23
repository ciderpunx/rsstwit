{-# LANGUAGE OverloadedStrings #-}
module FetchFeed where

import Network.HTTP.Conduit
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Control.Exception as X

-- Only title and link are used in the database.
data Postable = Postable { title :: T.Text
                         , link :: T.Text
                         , description :: T.Text
                         , pubDate :: T.Text
                         } deriving (Show)

type URI = T.Text

fetchFeed :: URI -> IO (Maybe [Postable])
fetchFeed uri = do
    src <- simpleHttp (T.unpack uri) `X.catch` eHandler
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

eHandler :: HttpException -> IO L.ByteString
eHandler e = putStrLn "Error occurred during download" >> return L.empty

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
