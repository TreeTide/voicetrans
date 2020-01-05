{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wave
    ( mkStreamer
    , Streamer, isFinishing, sendChunk, closeStream
    ) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Binary.Get (runGet)
import Data.Binary.IEEE754 (getFloat32le)
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Conduit (($$), (=$=), await, awaitForever, yield, ConduitM, Source)
import Data.Conduit.Audio
import Data.Conduit.Audio.SampleRate
import qualified Data.Conduit.List as C
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector.Binary (genericGetVectorWith, genericPutVectorWith)
import qualified Data.Vector.Generic as V

import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TBMQueue as Queue
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Conduit.TQueue as Queue

-- | Produces a wav header chunk, for now for fixed parameters.
-- TODO(robinp): plumb the params from the client.
wavHeader :: (Monad m) => Source m BS.ByteString
wavHeader = yield . B.toStrict . runPut $ do
    let sourceSampleRate = 44100
        sampleRate = 16000
        bitsPerSample = 16
        channels = 1
        byteRate = (sampleRate * bitsPerSample * channels) `div` 8
        sampleAlignment = (bitsPerSample * channels) `div` 8
        unknownSize = 0  -- we are streaming
    putByteString "RIFF"
    putWord32le unknownSize  -- of remaining
    putByteString "WAVE"
    putByteString "fmt "
    putWord32le 16 -- header size in bytes - TODO calc based on actual
    putWord16le 1  -- PCM
    putWord16le (fromIntegral channels)
    putWord32le (fromIntegral sampleRate)
    putWord32le (fromIntegral byteRate)
    putWord16le (fromIntegral sampleAlignment)
    putWord16le (fromIntegral bitsPerSample)
    putByteString "data"
    putWord32le unknownSize  -- data size

-- | Holds functionality to stream parts of a single recognition for async
-- processing.
data Streamer = Streamer
    { isFinishing :: IO Bool
      -- ^ True if the recognition is not accepting any more chunks.
    , sendChunk :: B.ByteString -> IO ()
      -- ^ Send the next chunk for async processing.
    , closeStream :: IO Text
      -- ^ Terminates the stream and returns the recognition result.
    }

-- | Creates a Streamer that can be used to send the chunks and eventually wait
-- for the result.
--
-- Behind the scenes concurrent queues are used to ship the the chunks to the
-- processing thread (feeding the chunks to a transformation pipeline)
mkStreamer :: (IO BS.ByteString -> IO Text) -> IO Streamer
mkStreamer recognizer = do
    -- The transformation pipeline sources from and sinks into queues.
    qs <- mk
    qt <- mk
    finishing <- atomically (TVar.newTVar False)
    let floatSampleVecs =
            Queue.sourceTBMQueue qs
                =$= C.map (\bs -> runGet (unserializeFloatVec (B.length bs)) bs)
        -- TODO(robinp): take sampling rates as arg.
        audioSrc = AudioSource
            { source = floatSampleVecs
            , rate = fromIntegral 44100
            , channels = 1
            , frames = 0  -- not known upfront
            }
        resampledSrc = resampleTo 16000 SincBestQuality audioSrc
        floatResampledBytes =
            source resampledSrc
                =$= displayPwr
                =$= C.map (runPut . serializeFloatVec)
                =$= awaitForever (C.sourceList . B.toChunks)
          where
            displayPwr = awaitForever $ \v -> do
                liftIO $ print (pwr v)
                yield $! v
              where
                pwr = sum . map (\(x :: Float) -> x*x) . V.toList
    -- Run pipeline in a thread.
    _ <- forkIO $ do
        print "Stream starts"
        _ <- runResourceT $!
                 (wavHeader >> floatResampledBytes)
                     $$ Queue.sinkTBMQueue qt True
        print "Stream done"
    -- Spawn a thread to call the recognition.
    -- TODO(robinp): Maybe form the recognizer as a conduit pipeline, so we
    --   don't have to work with ugly raw producers here, and could merge the
    --   two threads in one. Now the producer thing leaks to this level since
    --   the Network package can take chunked data that way and we don't do
    --   anything to wrap it a bit.
    stt <- Async.async . recognizer $ do
        chunk <- atomically (Queue.readTBMQueue qt)
        case chunk of
            Just _ -> print "Got chunk"
            Nothing -> print "Finished chunks"
        return $! fromMaybe BS.empty chunk
    -- The functions of the streamer, the client will use these.
    let fin = TVar.readTVarIO finishing
        send bs = atomically (Queue.writeTBMQueue qs bs)
        close = do
            print "Closing qs"
            atomically (Queue.closeTBMQueue qs)
            Async.wait stt  -- TODO(robinp): handle
    return $! Streamer fin send close
  where
    mk = atomically (Queue.newTBMQueue 8)


unserializeFloatVec byteSize = genericGetVectorWith
    (return . (`div` 4) . fromIntegral $ byteSize)
    getFloat32le

serializeFloatVec = genericPutVectorWith
    (const (return ()))
    (\f -> let b = fromIntegral (maxBound :: Int16)
               s = truncate (min 1.0 (max (-1.0) f) * b)
           in putWord16le s)
