{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Views for user registration.
module Guide.Views.Auth.Register where

import Imports

-- digestive-functors
import Text.Digestive
-- lucid
import Lucid hiding (for_)

import Guide.Config
import Guide.Types.User
import Guide.Views.Page
import Guide.Views.Utils

-- | Fields used by this form/view.
data UserRegistration = UserRegistration {
  registerUserName               :: Text,
  registerUserEmail              :: Text,
  registerUserPassword           :: Text,
  registerUserPasswordValidation :: Text }

-- | Creates a digestive functor over the fields in 'UserRegistration'
registerForm :: Monad m => Form (HtmlT (ReaderT Config IO) ()) m UserRegistration
registerForm = UserRegistration
  <$> "name" .: text Nothing
  <*> "email" .: text Nothing
  <*> "password" .: text Nothing
  <*> "passwordValidation" .: text Nothing

-- | Render input elements for a 'UserRegistration'
-- Note: This does not include the 'Form' element.
--
-- Use 'Guide.Server.protectForm' to render the appropriate form element with CSRF protection.
registerFormView :: MonadIO m => View (HtmlT m ()) -> HtmlT m ()
registerFormView view = do
  div_ $ do
    errorList "name" view
    label     "name" view "Name: "
    inputText "name" view

  div_ $ do
    errorList "email" view
    label     "email" view "Email: "
    inputText "email" view

  div_ $ do
    errorList     "password" view
    label         "password" view "Password: "
    inputPassword "password" view

  div_ $ do
    errorList     "passwordValidation" view
    label         "passwordValidation" view "Re-enter password: "
    inputPassword "passwordValidation" view

  inputSubmit "Register"

-- | Dummy for now.
registerView :: (MonadIO m) => User -> HtmlT m ()
registerView user = do
  div_ $ do
    -- TODO: Make nicer.
    "You are registered and logged in as "
    toHtml (userName user)

renderRegister :: (MonadIO m, MonadReader Config m) => HtmlT m () -> HtmlT m ()
renderRegister content = do
  renderPage $
    pageDef & pageTitle .~ "Aelve Guide"
            & pageContent .~ content
