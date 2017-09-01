{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{- |
Module      :  Guide.Auth.OAuth2
Description :  OAuth2 authentication module
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

{-# LANGUAGE TypeFamilies        #-}

module Guide.Auth.OAuth2 where

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
import           Guide.Auth.OAuth2.GitHub
-- import           Guide.Config.OAuth2
-- import           Guide.Routes
import           Guide.ServerStuff


mkOAuth2Api :: Path '[] 'Open -> GuideM ctx ()
mkOAuth2Api prefix = do
  _cfg <- getConfig
  _ <- mkGitHubAuth $ prefix <//> "github"
  -- Spock.get "foo" $ _foo
  return ()


-- mkAuthApi :: Path '[] 'Open -> GuideApp ()
-- mkAuthApi prefix = do
--     let OAuth2route r = prefix <//> "OAuth2" <//> r
--     cfg <- getSpockCfg
--     Spock.get (OAuth2route "google") undefined
