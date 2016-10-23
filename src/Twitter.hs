{-# LANGUAGE OverloadedStrings #-}

module Twitter where

import Prelude hiding (map)
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe (fromMaybe)

--  TODO load API deets from a config file
import TwitterCredsPrivate

-- test function, get timeline
getHomeTimeLine :: IO ()
getHomeTimeLine = do
    mgr <- newManager tlsManagerSettings
    timeline <- call twInfo mgr homeTimeline
    mapM_ (\s -> liftIO . print $ s ^. statusText) timeline

-- Todo: make twitter work

-- takes a T.Text and returns Just that text if tweeted and Nothing if not
tweetText :: T.Text -> IO ()
tweetText text = do
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr $ update text  -- add exception catch here
    return ()
