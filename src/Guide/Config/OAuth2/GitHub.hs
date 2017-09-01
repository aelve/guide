{- |
Module      :  Guide.Config.OAuth2.GitHub
Description :  GitHub OAuth2 authentication parameters & workflow.
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

{-# LANGUAGE OverloadedStrings #-}

module Guide.Config.OAuth2.GitHub where

import Imports

-- acid-state
-- import Data.SafeCopy hiding (kind)

-- import Guide.Utils
-- import Guide.Types.User
-- Aeson
import Data.Aeson
-- import Data.Aeson.Encode.Pretty
-- ByteString
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BSL
-- Default
import Data.Default

import Guide.Config.OAuth2.Base

-- | GitHub authentication details
data GitHubEndpoint = GitHubEndpoint {
  _githubBaseConfig :: OAuth2Endpoint
  }
  deriving (Eq, Show)

makeClassy ''GitHubEndpoint

instance Default GitHubEndpoint where
  def = GitHubEndpoint {
    _githubBaseConfig = def {
      _endpointName = "GitHub",
      _endpointAuthorize = "https://github.com/login/oauth/authorize?scope=user:email",
      _endpointAccessToken = "https://github.com/login/oauth/access_token"
    }
  }

instance FromJSON GitHubEndpoint where
  parseJSON (Object o) = do
    let def' = def :: GitHubEndpoint
    _githubBaseConfig <- do
          _endpointName         <- o .:? "name"             .!= (def' ^. endpointName)
          _endpointClientId     <- o .:? "client-id"        .!= (def' ^. endpointClientId)
          _endpointClientSecret <- o .:? "client-secret"    .!= (def' ^. endpointClientSecret)
          _endpointAuthorize    <- o .:? "authorize-url"    .!= (def' ^. endpointAuthorize)
          _endpointAccessToken  <- o .:? "access-token-url" .!= (def' ^. endpointAccessToken)
          _endpointCallback     <- o .:? "callback-url"     .!= (def' ^. endpointCallback)
          return $ OAuth2Endpoint {..}
    return $ GitHubEndpoint {..}

instance ToJSON GitHubEndpoint where
  toJSON GitHubEndpoint{..} =
    Object base
    where
      (Object base) = toJSON _githubBaseConfig


instance HasOAuth2Endpoint GitHubEndpoint where
  oAuth2Endpoint = githubBaseConfig
