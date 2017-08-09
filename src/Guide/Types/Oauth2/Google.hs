{- |
Module      :  Guide.Types.Oauth2.Google
Description :  Google Oauth2 authentication parameters & workflow.
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

{-# LANGUAGE OverloadedStrings #-}

module Guide.Types.Oauth2.Google where

import Imports

-- acid-state
import Data.SafeCopy hiding (kind)

-- import Guide.SafeCopy
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

-- | Google authentication details
data GoogleEndpoint = GoogleEndpoint {
  _googleEndpointConfig :: Oauth2Endpoint
  }
  deriving (Eq, Show)

makeClassy ''GoogleEndpoint

instance Oauth2Default GoogleEndpoint where
  oauth2def baseUrl = GoogleEndpoint {
    _googleEndpointConfig = (oauth2def baseUrl) {
      _endpointName = "Google",
      _endpointAuthorize = "https://accounts.google.com/o/oauth2/v2/auth",
      _endpointAccessToken = "https://www.googleapis.com/oauth2/v4/token"
    }
  }

instance HasOauth2Endpoint GoogleEndpoint where
  oauth2Endpoint = googleEndpointConfig