{- |
Module      :  Guide.Config.OAuth2.AccessToken
Description :  OAuth2 Access Token
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable | non-portable (<reason>)

moduleLongerDescription
-}

module Guide.Config.OAuth2.AccessToken where

import Imports

data AccessToken = AccessToken {
  _accessToken  :: Text             -- ^ The access token issued by the authorization server.
  _refreshToken :: Maybe Text       -- ^ The refresh token, which can be used to obtain new
                                    --    access tokens using the same authorization grant
  _expiresIn    :: Maybe Int        -- ^ The lifetime of the access token. Must be converted
                                    --    from seconds to a date upon receipt.
  _tokenType    :: Text             -- ^ The type of the token issued. Typically "Bearer"
  _scope        :: Maybe Text       -- ^ The scope of the access token.
  }

makeClassy ''AccessToken

instance FromJSON AccessToken where
  parseJSON (Object r) =
    AccessToken <$> r .:  "access_token"
                <*> r .:? "refresh_token"
                <*> r .:? "expires_in"
                <*> r .:  "token_type"
                <*> r .:? "scope"
