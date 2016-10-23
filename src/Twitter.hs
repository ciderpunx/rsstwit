{-# LANGUAGE OverloadedStrings #-}

module Twitter where

import Prelude hiding (map)
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import qualified Data.Text as T
import Control.Lens
import Control.Exception as X

--  TODO load API deets from a config file
import TwitterCredsPrivate

-- Takes a T.Text and returns Just that text if tweeted, 
-- Nothing otherwise
tweetText :: T.Text -> IO (Maybe T.Text)
tweetText t =
    tt t `X.catch` eHandler
  where
    tt text = do
      mgr <- newManager tlsManagerSettings
      res <- call twInfo mgr $ update text
      return $ res ^? statusText

eHandler :: TwitterError -> IO (Maybe T.Text)
eHandler e = return Nothing

---- test function, get timeline
--getHomeTimeLine :: IO ()
--getHomeTimeLine = do
--    mgr <- newManager tlsManagerSettings
--    timeline <- call twInfo mgr homeTimeline
--    mapM_ (\s -> liftIO . print $ s ^. statusText) timeline
