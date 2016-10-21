{-# LANGUAGE OverloadedStrings #-}
module FetchFeed where

import Network.HTTP.Conduit
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

data Postable = Postable { title :: T.Text
                         , link :: T.Text
                         , description :: T.Text
                         , pubDate :: T.Text } deriving (Show)

type URI = String

fetchFeed :: URI -> IO [Postable]
fetchFeed uri = do
    src <- simpleHttp uri
    let doc    = parseLBS_ def src
        cursor = fromDocument doc
        rss    =  cursor $.// laxElement "rss"
    return $
      if null rss -- we got a feed that doesn't look like an rss
      then map atomToPostable (cursor $// laxElement "entry")
      else map rssToPostable (cursor $// laxElement "item")

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
