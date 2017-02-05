-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Guide.Views.Auth.Register where

import Imports

import Lucid hiding (for_)

import Guide.Views.Page

import Guide.Config
import Guide.State
-- import Guide.Types
import Guide.Utils
import Guide.JS (JS(..), JQuerySelector)
import qualified Guide.JS as JS
import Guide.Markdown
import Guide.Cache


registerContent :: (MonadIO m, MonadReader Config m) => HtmlT m ()
registerContent =
  div_ ""

renderRegister :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderRegister =
  renderPage $ 
    pageDef & pageTitle .~ "Aelve Guide"
            & pageName .~ Just "Register"
            & pageContent .~ registerContent
