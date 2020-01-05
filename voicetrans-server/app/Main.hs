{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.STM                 (atomically)
import           Data.Monoid
import qualified Data.Text.Lazy                    as T
import           HFlags
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus.Extra                  as P
import qualified STMContainers.Map                 as STM
import           Web.Scotty

import           Lib
import           Translate
import           Wave

main :: IO ()
main = do
    -- HFlags is somewhat debated, but for now it is pretty ok.
    _ <- $initHFlags "VoiceTrans server"
    calls <- STM.newIO
    serve calls

-- TODO(robinp): metrics to submodule.
{-# NOINLINE processLatencyz #-}
processLatencyz :: P.Metric P.Summary
processLatencyz = P.unsafeMakeSummary "some:process_latency"
  "Latency of something"

serve calls = scotty 3000 $ do
    middleware (P.prometheus P.def)
    defaultHandler $ \e -> do
        liftIO (print e)  -- TODO(robinp): proper error handling
        text e
    -- Starts a new chunked recognition, returns the assigned id.
    -- Subsequent calls should refer that id.
    get "/namez" $
        text "voicetrans-server"
    post "/api/start" $ do
        -- TODO(robinp): generate a random one for each. If there were concurrent
        --   recognitions running, they would get mixed.
        let callIdent = "abcd"
        liftIO $ do
            call <- mkStreamer recognizeWaveChunks
            atomically . STM.insert call callIdent $ calls
        text callIdent
    -- Process next chunk of a recognition.
    post "/api/chunk" . P.measure processLatencyz $ do
        bs <- body
        liftIO $ do
            mbCall <- atomically (STM.lookup "abcd" calls)
            call <- case mbCall of
                Just c -> return $! c
                Nothing -> do
                    -- First chunk, create streamer.
                    -- Note: Could flag the STM stuff to avoid double fetch etc.
                    r <- mkStreamer recognizeWaveChunks
                    atomically (STM.insert r "abcd" calls)
                    return $! r
            sendChunk call bs
        text "continue"
    -- Finish recognition.
    post "/api/endchunk" $ do
        call <- liftIO . atomically . STM.lookup "abcd" $ calls
        res <- case call of
            Just c -> liftIO $ do
                atomically (STM.delete "abcd" calls)
                recognized <- closeStream c
                translated <- liftIO (translate recognized)
                return $! T.fromStrict (recognized <> " / " <> translated)
            Nothing -> return $! "Eh?"
        text res
