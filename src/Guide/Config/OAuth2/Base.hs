{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Guide.Config.OAuth2.Base
Description :  OAuth2 data definitions
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

module Guide.Config.OAuth2.Base where

import           Imports                  hiding ((.=))

-- hoauth2
import           Network.OAuth.OAuth2
-- text
import qualified Data.Text.All            as T
-- Aeson
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
-- Default
import           Data.Default

import           Guide.Utils


-- | Configuration details to access an endpoint.
--
-- Intended to be used in a config file.
--
-- See note [OAuth2 workflow]
--
-- Note that some of these parameters may be "read only" in the configuration,
-- and overwritten by an authorization provider.
data OAuth2Endpoint = OAuth2Endpoint {
  _endpointName         :: Text,      -- ^ A unique name for the OAuth2 endpoint.
  _endpointClientId     :: Text,      -- ^ The client ID for the OAuth2 endpoint.
  _endpointClientSecret :: Text,      -- ^ The client secret for the OAuth2 endpoint.
  _endpointAuthorize    :: Url,       -- ^ The endpoint authorization URL, clients are redirected
                                      --    to this URL to begin the OAuth2 workflow.
  _endpointCallback     :: Maybe Url, -- ^ The server callback URL, after logging in at the authorize URL
                                      --    the client is redirected to this server-controlled URL to provide
                                      --    the server with a
  _endpointAccessToken  :: Url        -- ^ The endpoint access token URL, once the app receives a
                                      --    authorization code, this endpoint may be used to obtain
                                      --    a longer-lived access token for an API.
  }
  deriving (Eq, Show)

makeClassy ''OAuth2Endpoint

instance Default OAuth2Endpoint where
  def = OAuth2Endpoint {
    _endpointName = "",
    _endpointClientId = "",
    _endpointClientSecret = "",
    _endpointAuthorize = "",
    _endpointAccessToken = "",
    _endpointCallback = Nothing -- baseUrl // "auth" // "OAuth2" // "callback"
  }

instance FromJSON OAuth2Endpoint where
  parseJSON = withObject "config" $ \o -> do
    _endpointName         <- o .:? "name"             .!= _endpointName def
    _endpointClientId     <- o .:? "client-id"        .!= _endpointClientId def
    _endpointClientSecret <- o .:? "client-secret"    .!= _endpointClientSecret def
    _endpointAuthorize    <- o .:? "authorize-url"    .!= _endpointAuthorize def
    _endpointAccessToken  <- o .:? "access-token-url" .!= _endpointAccessToken def
    _endpointCallback     <- o .:? "callback-url"     .!= _endpointCallback def
    return $ OAuth2Endpoint {..}


instance ToJSON OAuth2Endpoint where
  toJSON OAuth2Endpoint{..} = object [
    "name"             .= _endpointName,
    "client-id"        .= _endpointClientId,
    "client-secret"    .= _endpointClientSecret,
    "authorize-url"    .= _endpointAuthorize,
    "access-token-url" .= _endpointAccessToken,
    "callback-url"     .= _endpointCallback ]

-- TODO: Make class?
mkOAuth2 :: HasOAuth2Endpoint t => t -> OAuth2
mkOAuth2 cfg = OAuth2 {
  oauthClientId            = T.toByteString  $  cfg ^. endpointClientId,
  oauthClientSecret        = T.toByteString  $  cfg ^. endpointClientSecret,
  oauthOAuthorizeEndpoint  = T.toByteString  $  cfg ^. endpointAuthorize,
  oauthAccessTokenEndpoint = T.toByteString  $  cfg ^. endpointAccessToken,
  oauthCallback            = T.toByteString <$> cfg ^. endpointCallback
  }

{- Note [OAuth2 workflow]

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

class HasOAuth2Endpoint t where endpointName :: ...

instance HasOAuth2Endpoint OAuth2Endpoint where
  ...

Each instance of an OAuth2 endpoint then will contain a OAuth2Endpoint field,
and implement the above type classes.

-}
