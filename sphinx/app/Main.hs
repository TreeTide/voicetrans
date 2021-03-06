module Main where

import Sphinx
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BS
import Foreign.Ptr (castPtr, nullPtr)

main :: IO ()
main = do
  -- NOTE: from https://github.com/cmusphinx/pocketsphinx/tree/master/test/data
  raw <- BS.readFile "/tmp/goforward.raw"
  decode raw

decode bs = do
  args <- defaultAcceptedArgs
  -- NOTE: download en-us model from https://github.com/cmusphinx/pocketsphinx/tree/master/model/en-us
  let mdir = "/tmp/pocketsphinx/model/en-us"
  -- TODO(robinp): bracket etc
  cfg <- initConfig3 (Config nullPtr) args True
           "-hmm" (mdir ++ "/en-us")
           "-lm" (mdir ++ "/en-us.lm.bin")
           "-dict" (mdir ++ "/cmudict-en-us.dict")
  dec <- Sphinx.initDecoder cfg
  print "Starting..."
  startUtt dec
  forM_ [bs, bs, bs] $ \dat0 -> do
    forM_ (chunk 2048 dat0) $ \dat -> do
      num <- BS.unsafeUseAsCStringLen dat $ \(str, n) ->
                processRaw dec (Samples (castPtr str)) (n `div` 2) False False
      print num
  endUtt dec
  r <- getHyp dec
  print r
  freeDecoder dec
  freeConfig cfg

chunk :: Int -> BS.ByteString -> [BS.ByteString]
chunk n = go
  where
    go bs | BS.null bs = []
          | otherwise = let (a,b) = BS.splitAt n bs in a:go b
