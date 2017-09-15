{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Guide.Config.OAuth2.Github
Description :  Github OAuth2 authentication parameters & workflow.
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable | non-portable (<reason>)

-}

module Guide.Config.OAuth2.Github where

import Imports hiding ((.=))

-- Aeson
import           Data.Aeson
-- Default
import           Data.Default
-- Text
import qualified Data.Text.All as T
-- unordered-containers
import qualified Data.HashMap.Lazy as HashMap

import           Guide.Config.OAuth2.Base
import           Guide.Utils

-- | Github authentication details
data GithubEndpoint = GithubEndpoint {
  _githubBaseConfig :: OAuth2Endpoint,
  _githubScopes     :: [Text]
  }
  deriving (Eq, Show)

makeClassy ''GithubEndpoint

-- Constants
defAuthorize, defAccessToken :: Url
defAuthorize = "https://github.com/login/oauth/authorize?scope="
defAccessToken = "https://github.com/login/oauth/access_token"


instance Default GithubEndpoint where
  def =
    GithubEndpoint {
      _githubScopes = defaultScopes,
      _githubBaseConfig = def {
        _endpointName = "Github",
        _endpointAuthorize = defAuthorize <> T.intercalate "," defaultScopes,
        _endpointAccessToken = defAccessToken
      }
    }
    where
      defaultScopes = ["user:email"]

instance FromJSON GithubEndpoint where
  parseJSON (Object o) = do
    let def' = def :: GithubEndpoint
    _githubScopes <- o .:? "scopes" .!= (def' ^. githubScopes)
    _githubBaseConfig <- do
      _endpointName         <- o .:? "name"             .!= (def' ^. endpointName)
      _endpointClientId     <- o .:? "client-id"        .!= (def' ^. endpointClientId)
      _endpointClientSecret <- o .:? "client-secret"    .!= (def' ^. endpointClientSecret)
      let _endpointAuthorize   = defAuthorize <> T.intercalate "," _githubScopes
      let _endpointAccessToken = defAccessToken
      _endpointCallback     <- o .:? "callback-url"     .!= (def' ^. endpointCallback)
      return $ OAuth2Endpoint {..}
    return $ GithubEndpoint {..}

instance ToJSON GithubEndpoint where
  toJSON GithubEndpoint{..} =
    Object $ ext `HashMap.union` base
    where
      (Object base) = toJSON _githubBaseConfig
      (Object ext) = object [
        "scopes" .= _githubScopes ]

instance HasOAuth2Endpoint GithubEndpoint where
  oAuth2Endpoint = githubBaseConfig
