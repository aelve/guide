{-# LANGUAGE OverloadedStrings #-}


-- | Functions for interacting with Matomo (<https://matomo.org/>, our web
-- analytics).
--
-- Matomo docs: <https://developer.matomo.org/api-reference/tracking-api>.
module Guide.Matomo
(
  Matomo(..),
  postMatomo,
)
where

import Imports

import Control.Concurrent.Async (async)
import Control.Monad.Extra (whenJust)
import Data.IP (IP)
import Network.HTTP.Client (httpLbs, parseRequest, setQueryString)
import Network.HTTP.Client.TLS (getGlobalManager)

import Guide.Api.Guider (Context (..), Guider)
import Guide.Config (Config (..))
import Guide.Types.Edit (Edit (..))
import Guide.Utils (Url)
import Guide.Logger

import qualified Data.ByteString as BS


-- | Request with log data to send to Matomo
data Matomo = Matomo
  { mIP       :: Maybe IP
  , mUA       :: Maybe Text
  , mReferrer :: Maybe Text
  , mTag      :: Edit
  } deriving (Eq, Show)

-- | Notify Matomo that an edit has been made.
postMatomo :: Matomo -> Guider ()
postMatomo Matomo{..} = push "postMatomo" $ do
    Context Config{..} _ _ <- ask
    whenJust matomoLink $ \matomo -> liftIO $
      void $ async $ do
        manager <- getGlobalManager
        req <- setQueryString
          [ ("idsite", Just "1")  -- The ID of the website we're tracking a visit/action for.
          , ("rec", Just "1")  -- Required for tracking, must be set to one.
          , ("url", toUtf8ByteString . show <$> mIP) -- The full URL for the current action.
          , ("ua", toUtf8ByteString <$> mUA) -- An override value for the User-Agent HTTP header field.
          , ("urlref", toUtf8ByteString <$> mReferrer) -- The full HTTP Referrer URL.
          , ("action_name", action_name) --  The title of the action being tracked.
          ] <$> parseRequest (piwik matomo)
        -- TODO: log if the request to Matomo has failed
        httpLbs req manager
    pure ()
  where
    action_name :: Maybe ByteString
    action_name = Just (BS.intercalate "/" ["Haskell", "Edit", showConstructor mTag])
    showConstructor :: Edit -> ByteString
    showConstructor = toUtf8ByteString
      . drop (length ("Edit" :: String))
      . takeWhile (not . isSpace)
      . show
    piwik :: Url -> String
    piwik matomo = format "POST {}" matomo
