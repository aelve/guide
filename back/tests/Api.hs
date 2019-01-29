{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api where

import BasePrelude

import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Control.Monad.Catch

import Guide.Api.Types
import qualified Test.Hspec as H

apiTests = do
  H.it "api: get categories request" $ do
    request <- makeRequest
      (Host "http://localhost/categories")
      (Port 4400)
      (Method "GET")
    _ :: [CCategoryInfo] <- runRequest request
    threadDelay 100000
    pure ()

runRequest :: Yaml.FromJSON a => Request -> IO a
runRequest request = getResponseBody <$> httpJSON request

newtype Host   = Host String
newtype Port   = Port Int
newtype Method = Method S8.ByteString

makeRequest :: MonadThrow m => Host -> Port -> Method -> m Request
makeRequest (Host host) (Port port) (Method method) = do
  initReq <- parseRequest host
  pure $
    setRequestPort port $
    setRequestMethod method initReq