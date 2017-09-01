{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  Guide.Auth
Description :  Authentication module
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE TypeFamilies        #-}

module Guide.Auth where

-- Web
-- import           Lucid                         hiding (for_)
-- import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Web.Routing.Combinators       (PathState (Open))
import           Web.Spock                     hiding (get, head, text)
-- import qualified Web.Spock                     as Spock
-- import           Web.Spock.Config
-- import           Web.Spock.Lucid

import           Imports

import           Guide.App
import           Guide.Auth.OAuth2
-- import           Guide.Config
-- import           Guide.Config.OAuth2
-- import           Guide.Routes
-- import           Guide.ServerStuff

mkAuthApi :: Path '[] 'Open -> GuideM ctx ()
mkAuthApi prefix = do
  mkOAuth2Api $ prefix <//> "oauth2"

