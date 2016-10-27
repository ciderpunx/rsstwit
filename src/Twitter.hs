{-# LANGUAGE OverloadedStrings #-}

module Twitter (tweetText) where

import Control.Exception as X
import Control.Lens
import Prelude hiding (map)
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import qualified Data.Text as T

import Config

-- Takes a T.Text and returns Just that text if tweeted, 
-- Nothing otherwise
tweetText :: T.Text -> IO (Maybe T.Text)
tweetText t =
    tt t `X.catch` eHandler
  where
    tt text = do
      mgr <- newManager tlsManagerSettings
      twInfo <- getTwinfo
      res <- call twInfo mgr $ update text
      return $ res ^? statusText

eHandler :: TwitterError -> IO (Maybe T.Text)
eHandler e = do
    print $ "Error: problem tweeting\n\t" ++ show e
    return Nothing

---- test function, get timeline
--getHomeTimeLine :: IO ()
--getHomeTimeLine = do
--    mgr <- newManager tlsManagerSettings
--    timeline <- call twInfo mgr homeTimeline
--    mapM_ (\s -> liftIO . print $ s ^. statusText) timeline
