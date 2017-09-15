{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE TypeFamilies        #-}

{- |
Module      :  Guide.Auth
Description :  Authentication module
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

module Guide.Auth where

import           Imports

-- Web
import           Web.Routing.Combinators       (PathState (Open))
import           Web.Spock                     hiding (get, head, text)

import           Guide.App
import           Guide.Auth.OAuth2

-- | This function adds handlers for all authentication routes that
-- we need to support. Assuming that the prefix is @auth@, 'mkAuthApi'
-- will add handlers for these routes:
--
-- @
-- /auth/oauth2/github/...
-- /auth/oauth2/google/...
-- /auth/oauth2/generic/...
-- @
--
-- For details on the created routes, see 'mkOAuth2Api', 'mkGithubAuth', etc.
mkAuthApi :: Path '[] 'Open -> GuideM ctx ()
mkAuthApi prefix = do
  mkOAuth2Api $ prefix <//> "oauth2"

