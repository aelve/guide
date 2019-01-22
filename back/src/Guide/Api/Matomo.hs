{-# LANGUAGE OverloadedStrings #-}


{- | Matomo curl requests -}
module Guide.Api.Matomo
       ( Matomo(..)
       , postMatomo
       ) where

import Data.IP (IP)
import Imports
import Network.HTTP.Client (httpLbs, newManager, parseRequest, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Guide.Api.Guider (Guider)
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
postMatomo Matomo{..} = liftIO $ do
    manager  <- liftIO $ newManager tlsManagerSettings
    req <- setQueryString
      [ ("idsite", Just "1")
      , ("rec", Just "1")
      , ("url", toByteString . show <$> mIP)
      , ("ua", toByteString <$> mUA)
      , ("urlref", toByteString <$> mReferrer)
      , ("action_name", Just . toByteString $ show mTag)
      ] <$> parseRequest piwik
    _ <- httpLbs req manager
    pure ()
  where
    piwik = "POST http://localhost:8080/piwik.php"
