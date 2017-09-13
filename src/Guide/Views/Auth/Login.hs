{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


{- |
Views for login.
-}
module Guide.Views.Auth.Login where

import Imports

-- import Web.Routing.Combinators       (PathState (Open))
-- import           Web.Spock                     hiding (get, head, text)
-- digestive-functors
import Text.Digestive hiding (Path)
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
loginFormView :: (MonadIO m, MonadReader Config m) => View (HtmlT m ()) -> HtmlT m ()
loginFormView view' = do
  -- config :: Config <- ask
  div_ [class_ "auth"] $ do
    errorList "email" view'
    label     "email" view' "Email: "
    inputText "email" view'

  div_ $ do
    errorList     "password" view'
    label         "password" view' "Password: "
    inputPassword "password" view'

  inputSubmit "Log in"

  hr_ []

  div_ [class_ "federated"] $ do
    p_ "Or sign in with:"
    -- TODO: Generate this list from the current configuration
    -- will need to change type to our app or pass in as arg
    div_ [class_ "logos"] $ do
      -- TODO: These paths are hardcoded, should change this
      -- a_ [href_ "/auth/"] $
      --   img_ [src_ "/auth_logos/azure.png"]
      -- a_ [href_ "tbd"] $
      --   img_ [src_ "/auth_logos/facebook.png"]
      a_ [href_ "/auth/oauth2/github/forward"] $
        img_ [src_ "/auth_logos/github.png"]
      -- a_ [href_ "tbd"] $
      --   img_ [src_ "/auth_logos/google.png"]

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
