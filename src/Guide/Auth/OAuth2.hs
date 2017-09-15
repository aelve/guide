{-# LANGUAGE DataKinds           #-}

{- |
Module      :  Guide.Auth.OAuth2
Description :  OAuth2 authentication module
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

module Guide.Auth.OAuth2
(
  mkOAuth2Api
)
where


import           Imports

-- Web
import           Web.Routing.Combinators       (PathState (Open))
import           Web.Spock                     hiding (get, head, text)

import           Guide.App
import           Guide.Auth.OAuth2.Github
import           Guide.ServerStuff

mkOAuth2Api :: Path '[] 'Open -> GuideM ctx ()
mkOAuth2Api prefix = do
  mkGithubAuth $ prefix <//> "github"
  return ()
