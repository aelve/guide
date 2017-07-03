{- |
Module      :  Guide.Types.Oauth2.Config
Description :  Oauth2 data definitions
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE OverloadedStrings #-}

module Guide.Types.Oauth2.Config where

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
import Data.Default

-- | Configuration details to access an endpoint.
--
-- Intended to be used in a config file.
-- 
-- See note [Oauth2 workflow]
data Oauth2Endpoint = Oauth2Endpoint {
  _endpointName         :: Text,      -- ^ A unique name for the Oauth2 endpoint.
  _endpointClientId     :: Text,      -- ^ The client ID for the Oauth2 endpoint.
  _endpointClientSecret :: Text,      -- ^ The client secret for the Oauth2 endpoint.
  _endpointAuthorize    :: Url,       -- ^ The endpoint authorization URL, clients are redirected
                                      --    to this URL to begin the Oauth2 workflow.
  _endpointCallback     :: Url,       -- ^ The server callback URL, after logging in at the authorize URL
                                      --    the client is redirected to this server-controlled URL to provide
                                      --    the server with a 
  _endpointAccessToken  :: Url        -- ^ The endpoint access token URL, once the app receives a
                                      --    authorization code, this endpoint may be used to obtain
                                      --    a longer-lived access token for an API.
  }
  deriving (Eq, Show)

makeClassy ''Oauth2Endpoint

class Oauth2Default t where
  oauth2def :: Url -> t

instance Oauth2Default Oauth2Endpoint where
  oauth2def baseUrl = Oauth2Endpoint {
    _endpointName = "",
    _endpointClientId = "",
    _endpointClientSecret = "",
    _endpointAuthorize = "",
    _endpointAccessToken = "",
    _endpointCallback = baseUrl // "auth" // "oauth2" // "callback"
  }


  -- oauth2login :: t -> 

{- Note [Oauth2 workflow]

Terms:

* Endpoint: the third party authentication/authorization service.
* Server: this application (the Aelve Guide)
* Client: the user agent accessing the application (e.g.: web browser, API
  client)

One of the better sources on authentication workflow is Google's: 

https://developers.google.com/identity/protocols/OAuth2

Every OAuth2 provider is free to add additional parameters, so a general OAuth2
flow isn't possible. We use type classes and the `microlens` function
'makeClassy' to create type classes as such:

class HasOauth2Endpoint t where endpointName :: ...

instance HasOauth2Endpoint Oauth2Endpoint where
  ...

Each instance of an Oauth2 endpoint then will contain a Oauth2Endpoint field,
and implement the above type classes. 

-}