{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
module Translate
  ( translate
  ) where

import           Control.Lens
import           Data.Aeson.Lens (key, nth, _String)
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           HFlags
import           Network.Wreq

defineFlag
    "google_translate_apikey" ("" :: Text)
    "The secret key of the GCE project having Translate access."

serviceUrl :: Text
serviceUrl = "https://www.googleapis.com/language/translate/v2"

-- | Translates text between hardcoded laguages, for now.
translate :: Text -> IO Text
translate txt = do
  let opts = defaults
          & param "key" .~ [flags_google_translate_apikey]
          & param "target" .~ ["en"]
          & param "q" .~ [txt]
  r <- getWith opts (T.unpack serviceUrl)
  print r
  return $! fromMaybe "error"
    (r ^? responseBody . key "data" . key "translations" . nth 0
        . key "translatedText" . _String)


{- Gogol lib has trouble with Translate, so we roll our own.
See brendanhay/gogol/issues/19.
import Control.Monad.Trans.Resource
import Network.Google (Env, newEnv, runGoogle, send)
import Network.Google.Auth (fromFilePath)
import Network.Google.Translate

trans = do
  let req = translationsList ["Guten tag"] "hu"
  -- Credentials file need to be passed in GOOGLE_APPLICATION_CREDENTIALS env.
  env <- newEnv :: IO (Env '["https://www.googleapis.com/auth/cloud-platform"])
  res <- runResourceT (runGoogle env (send req))
  print res
  return $! res
-}
