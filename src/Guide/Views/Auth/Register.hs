{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Guide.Views.Auth.Register where

import Imports

import Lucid hiding (for_)

import Guide.Views.Page
import Guide.Config


registerContent :: (MonadIO m) => HtmlT m ()
registerContent =
  div_ ""

renderRegister :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderRegister =
  renderPage $ 
    pageDef & pageTitle .~ "Aelve Guide"
            & pageName .~ Just "Register"
            & pageContent .~ registerContent
