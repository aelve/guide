{- |
Module      :  Guide.Types.Oauth2.Github
Description :  Github Oauth2 authentication parameters & workflow.
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

{-# LANGUAGE OverloadedStrings #-}

module Guide.Types.Oauth2.Github where
  
import Imports

-- acid-state
import Data.SafeCopy hiding (kind)

import Guide.SafeCopy
import Guide.Utils
import Guide.Types.User
-- Aeson
import Data.Aeson
import Data.Aeson.Encode.Pretty
-- ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Default
import Guide.Types.Oauth2.Config

-- | Github authentication details
data GithubEndpoint = GithubEndpoint {
  _githubEndpointConfig :: Oauth2Endpoint
  }
  deriving (Eq, Show)

makeClassy ''GithubEndpoint

instance Oauth2Default GithubEndpoint where
  oauth2def baseUrl = GithubEndpoint {
    _githubEndpointConfig = (oauth2def baseUrl) {
      _endpointName = "Github",
      _endpointAuthorize = "http://github.com/login/oauth/authorize",
      _endpointAccessToken = "https://github.com/login/oauth/access_token"
    }
  }

instance HasOauth2Endpoint GithubEndpoint where
  oauth2Endpoint = githubEndpointConfig