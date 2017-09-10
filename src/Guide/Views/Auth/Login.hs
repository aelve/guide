{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


{- |
Views for login.
-}
module Guide.Views.Auth.Login where

import Imports

-- digestive-functors
import Text.Digestive
-- lucid
import Lucid hiding (for_)
import Guide.Views.Page
import Guide.Views.Utils
import Guide.Config
import Guide.Types.User

-- | Fields used by this form.
data Login = Login {
  loginEmail :: Text,
  loginUserPassword :: Text }

-- | Creates a digestive functor over the fields in 'UserRegistration'
loginForm :: Monad m => Form (HtmlT (ReaderT Config IO) ()) m Login
loginForm = Login
  <$> "email" .: text Nothing
  <*> "password" .: text Nothing

-- | Render input elements for a 'Login'
-- Note: This does not include the 'Form' element.
--
-- Use 'Guide.Server.protectForm' to render the appropriate form element with CSRF protection.
loginFormView :: MonadIO m => View (HtmlT m ()) -> HtmlT m ()
loginFormView view' = do
  div_ $ do
    errorList "email" view'
    label     "email" view' "Email: "
    inputText "email" view'

  div_ $ do
    errorList     "password" view'
    label         "password" view' "Password: "
    inputPassword "password" view'

  inputSubmit "Log in"

-- | Dummy for now.
loginView :: (MonadIO m) => User -> HtmlT m ()
loginView user = do
  div_ $ do
    -- TODO: Make nicer.
    "You are registered and logged in as "
    toHtml (user ^. userName)

renderLogin :: (MonadIO m, MonadReader Config m) => HtmlT m () -> HtmlT m ()
renderLogin content = do
  renderPage $
    pageDef & pageTitle .~ "Aelve Guide"
            & pageContent .~ content
