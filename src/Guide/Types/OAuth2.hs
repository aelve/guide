{- |
Module      :  Guide.Types.OAuth2
Description :  OAuth2 data definitions
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

module Guide.Types.OAuth2
    ( module X )
where

import Guide.App

import qualified Guide.Config.OAuth2.GitHub as X
import qualified Guide.Config.OAuth2.Google as X
import qualified Guide.Config.OAuth2 as X

class OAuth2Provider t where
    mkOAuth2 :: t -> OAuth2
