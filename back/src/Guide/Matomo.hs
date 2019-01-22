{-# LANGUAGE OverloadedStrings #-}


{- | Functions for interacting with Matomo (<https://matomo.org/>, our web analytics). -}
module Guide.Matomo
       ( Matomo(..)
       , postMatomo
       ) where

import Imports

import Data.IP (IP)
import Network.HTTP.Client (httpLbs, newManager, parseRequest, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Concurrent.Async (withAsync)

import Guide.Api.Guider (Context (..), Guider)
import Guide.Config (Config (..))
import Guide.Types.Edit (Edit (..))


-- | Request with log data to send to Matomo
data Matomo = Matomo
  { mIP       :: Maybe IP
  , mUA       :: Maybe Text
  , mReferrer :: Maybe Text
  , mTag      :: Edit
  } deriving (Eq, Show)

-- | Notify Matomo that an edit has been made.
postMatomo :: Matomo -> Guider ()
postMatomo Matomo{..} = do
    Context Config{..} _ _ <- ask
    case (_portMatomo, _pageMatomo) of
      (Just port, Just page) -> liftIO $ do
        flip withAsync (\_ -> pure ()) $ do
          -- TODO: pushing log to matomo is failed
          manager <- newManager tlsManagerSettings
          req <- setQueryString
            [ ("idsite", Just "1")  -- The ID of the website we're tracking a visit/action for.
            , ("rec", Just "1")  -- Required for tracking, must be set to one.
            , ("url", toByteString . show <$> mIP) -- The full URL for the current action.
            , ("ua", toByteString <$> mUA) -- An override value for the User-Agent HTTP header field.
            , ("urlref", toByteString <$> mReferrer) -- The full HTTP Referrer URL.
            , ("action_name", Just $ showConstructor mTag) --  The title of the action being tracked.
            ] <$> parseRequest (piwik port page)
          _ <- httpLbs req manager
          pure ()
      _ -> pure ()

  where
    showConstructor :: Edit -> ByteString
    showConstructor = toByteString . takeWhile (not . isSpace) . show
    piwik :: Int -> Text -> String
    piwik port page = format "POST http://localhost:{}/{}" port page

