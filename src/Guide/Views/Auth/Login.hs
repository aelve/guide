{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Guide.Views.Auth.Login where

import Imports

import Lucid hiding (for_)

import Guide.Views.Page
import Guide.Config


loginContent :: (MonadIO m) => HtmlT m ()
loginContent = do
  div_ ""

renderLogin :: (MonadIO m, MonadReader Config m) => HtmlT m ()
renderLogin = do
  renderPage $ 
    pageDef & pageTitle .~ "Aelve Guide"
            & pageName .~ Just "Login"
            & pageContent .~ loginContent
