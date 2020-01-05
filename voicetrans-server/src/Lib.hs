{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{- Speech to text module using MS API. -}
module Lib
    ( recognizeWaveLBS
    , recognizeWaveChunks
    ) where

import           Control.Lens
import           Data.Aeson.Lens      (key, nth, _Integer, _String)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           HFlags
import           Network.HTTP.Client  (RequestBody (RequestBodyLBS, RequestBodyStreamChunked))
import           Network.Wreq
import           Safe                 (readMay)
import           System.IO.Unsafe     (unsafePerformIO)

import           Refresh              (timeCachedToken)

-- TODO(robinp): move flags to different submodule to avoid TH on this one?
defineFlag
    "ms_stt_secret_key" ("" :: Text)
    "The secret key of the Azure project having speech-to-text."

-- | Url of speech api.
bingSpeechBaseUrl :: Text
bingSpeechBaseUrl = "https://speech.platform.bing.com"

-- | Url of access token service.
issueTokenUrl :: Text
issueTokenUrl = "https://oxford-speech.cloudapp.net/token/issueToken"

-- | Retrieves a new token along with expiry delta in seconds.
getToken :: IO (Text, Double)
getToken = do
    -- WTF: the returned JWT token has the secret key plain Base64-encoded?
    r <- post (T.unpack issueTokenUrl)
        [ "grant_type"    := asTxt "client_credentials"
        , "client_id"     := asTxt "unused"
        , "client_secret" := flags_ms_stt_secret_key
        , "scope"         := bingSpeechBaseUrl
        ]
    print r
    let expiry_seconds = (readMay . T.unpack)
            (r ^. responseBody . key "expires_in" . _String) :: Maybe Int
        token = r ^. responseBody . key "access_token" . _String
    return $! (token, maybe 0 fromIntegral expiry_seconds)
  where
    -- | Just for fixing the type.
    asTxt :: Text -> Text
    asTxt = id

-- | Action returning a usable token. Caches and refreshes internally.
-- TODO(robinp): use service pattern to avoid global.
getCachedToken :: IO Text
{-# NOINLINE getCachedToken #-}
getCachedToken = unsafePerformIO (timeCachedToken getToken snd fst)

-- | Recognizes a wave file provided as lazy ByteString.
recognizeWaveLBS :: BL.ByteString -> IO Text
recognizeWaveLBS bs = recognizeBody (RequestBodyLBS bs)

-- | Recognizes a wave file provided by a chunk generator.
recognizeWaveChunks
    :: IO ByteString  -- ^ Generator of chunks. Empty result means end.
    -> IO Text
recognizeWaveChunks gen = recognizeBody (RequestBodyStreamChunked ($ gen))

-- | Returns the recognized text. TODO(robinp): error handling.
recognizeBody :: RequestBody -> IO Text
recognizeBody requestBody = do
    token <- getCachedToken
    -- TODO(robinp): logging-effect instead of print
    print ("Got cached token: ", token)
    let rand = "b2c95ede-97eb-4c88-81e4-80f32d6aee54"  -- not really
        url = bingSpeechBaseUrl <> "/recognize/query"
    let opts = defaults
          & header "Authorization" .~ [T.encodeUtf8 ("Bearer " <> token)]
          & param "Version" .~ ["3.0"]
          & param "requestid" .~ [rand]
          & param "appID" .~ ["D4D52672-91D7-4C74-8AD8-42B1D98141A5"]  -- fixed
          & param "format" .~ ["json"]
          & param "locale" .~ ["de-DE"]  -- TODO(robinp): parametrize on this.
          & param "device.os" .~ ["Linux OS"]
          & param "scenarios" .~ ["ulm"]  -- ?
          & param "instanceid" .~ [rand]
          & param "maxnbest" .~ ["3"]  -- no effect?
    print "Starting to post"
    s <- postWith opts (T.unpack url)
        (Raw ("audio/wav; samplerate=16000; "
              <> "sourcerate=44100; trustsourcerate=true") requestBody)
    print ("Finished post", s)
    let result =
            s ^? responseBody . key "results" . nth 0 . key "lexical" . _String
    return $! fromMaybe "...error..." result
