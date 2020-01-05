{-# LANGUAGE OverloadedStrings #-}
{- Module for getting tokens with various refresh strategies. -}
module Refresh
  ( cachedToken
  , timeCachedToken
  ) where

import Control.Concurrent.MVar
import Data.AdditiveGroup ((^+^))
import Data.Thyme.Clock (fromSeconds)
import Data.Thyme.Clock.POSIX
import qualified Prometheus.Extra as P

-- TODO(robinp): move monitoring to submodule.
{-# NOINLINE refreshCountz #-}
refreshCountz :: P.Metric P.Counter
refreshCountz = P.unsafeMakeCounter "some:refresh_count" "Token refresh count"

-- | Caches token sourced from a producer, and automatically refreshes the token
-- once it gets invalid.
cachedToken
    :: IO a            -- ^ Action to produce new token data.
    -> (a -> IO Bool)  -- ^ Action validating the token.
    -> (a -> b)        -- ^ How to extract token from data.
    -> IO (IO b)
cachedToken retrieve isValid extract = do
    holder <- ret >>= newMVar
    return $! modifyMVar holder $ \tok -> do
        valid <- isValid tok
        if valid
           then use tok
           else ret >>= use
  where
    use a = return $! (a, extract a)
    ret = P.postIncrement refreshCountz retrieve

-- | Caches and refreshes a token which gets invalid by time expiry.
timeCachedToken
    :: IO a           -- ^ Action to produce new token data.
    -> (a -> Double)  -- ^ Seconds to expire from time of retrieving.
    -> (a -> b)       -- ^ How to extract token from data.
    -> IO (IO b)
timeCachedToken retrieve ttlSeconds extract =
    cachedToken retrieveWithAbsoluteExpiry checkExpiry unwrapExtract
  where
    retrieveWithAbsoluteExpiry = do
        time <- getPOSIXTime
        tok <- retrieve
        print ("Retrieve", time, "expiry", ttlSeconds tok)
        return $! (tok, time ^+^ fromSeconds (ttlSeconds tok))
    checkExpiry (_, deadline)= do
        now <- getPOSIXTime
        print ("Check", now, deadline)
        return $! now < deadline
    unwrapExtract (tok, _) = extract tok

-- TODO(robinp): explicit MVar versions.
-- TODO(robinp): preemtive time-cache refresh strategy.
