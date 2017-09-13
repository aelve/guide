{-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeOperators       #-}

{- |
Module      :  Guide.Auth.OAuth2.GitHub
Description :  GitHub authentication module
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable
Portability :  portable

-}

module Guide.Auth.OAuth2.GitHub where

import Imports

-- text
import qualified Data.Text.All as T
-- JSON
import Data.Aeson               as Aeson
-- import Data.Aeson.Encode.Pretty as Aeson hiding (Config)
-- Map
import qualified Data.Map as Map

-- Web
-- import           Lucid                         hiding (for_)
-- import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Web.Routing.Combinators       (PathState (Open))
import           Web.Spock                     hiding (get, head, text)
import qualified Web.Spock                     as Spock
-- import           Web.Spock.Config
-- import           Web.Spock.Lucid

-- import           Imports

import           Guide.App
-- import           Guide.Auth.OAuth2.GitHub
import           Guide.Config
import           Guide.Config.OAuth2
import           Guide.Handlers
-- import           Guide.Routes
import           Guide.ServerStuff
import           Guide.Types.Session
import           Guide.Types.Creds
import           Guide.Types.User

-- import           Guide.Views.Utils

import Guide.State

import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.OAuth.OAuth2

data GithubUser = GithubUser
  { githubUserId :: Int
  , githubUserName :: Maybe Text
  , githubUserLogin :: Text
  , githubUserAvatarUrl :: Text
  , githubUserLocation :: Maybe Text
  , githubUserPublicEmail :: Maybe Text
  }

instance FromJSON GithubUser where
  parseJSON (Object o) = GithubUser
      <$> o .: "id"
      <*> o .:? "name"
      <*> o .: "login"
      <*> o .: "avatar_url"
      <*> o .:? "location"
      <*> o .:? "email"

  parseJSON _ = mzero

data GithubUserEmail = GithubUserEmail
  { githubUserEmailAddress :: Text
  , githubUserEmailPrimary :: Bool
  }

instance FromJSON GithubUserEmail where
  parseJSON (Object o) = GithubUserEmail
      <$> o .: "email"
      <*> o .: "primary"

  parseJSON _ = mzero


mkGitHubAuth :: Path '[] 'Open -> GuideM ctx ()
mkGitHubAuth prefix = do
  cfg <- getConfig
  mgr <- getManager
  baseUrl <- _baseUrl <$> getConfig
  case _githubOauth cfg of
    Just githubCfg -> do
        setupForward
        setupCallback
      where
        oauth = mkOAuth2 githubCfg
        forwardRoute = prefix <//> "forward"
        callbackRoute = prefix <//> "callback"
        withCallback csrfValue =
          oauth {
              oauthCallback = Just . T.toByteString $ baseUrl <> renderRoute callbackRoute,
              oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth
                `appendQueryParam` [(T.encodeUtf8 csrfTokenParam, T.encodeUtf8 csrfValue)]
            }
        setupForward = do
          Spock.get forwardRoute $ do
            csrfValue <- getCsrfToken
            let authUrl = T.decodeUtf8 . authorizationUrl $ withCallback csrfValue
            Spock.redirect authUrl
        setupCallback = Spock.getpost callbackRoute $ checkCallbackCsrf $ do
          code :: Text <- param' "code"
          csrfValue <- getCsrfToken
          let oauth' = withCallback csrfValue
          result <- liftIO $ fetchAccessToken mgr oauth' (T.encodeUtf8 code)
          case result of
            Left _ -> errorInvalidToken
            Right token -> do
              mbCreds <- liftIO $ fetchGithubProfile mgr token
              case mbCreds of
                Just creds -> do
                  mbCurrentUser <- getLoggedInUser
                  -- TODO: This is hackish, need to create a "profile" page and
                  -- an "associate login" page.
                  case mbCurrentUser of
                    Just currentUser -> do
                      result <- dbUpdate $ AddCreds currentUser creds
                      if result
                      then Spock.text "Associated account"
                      else Spock.text "Failed to associate account"
                    Nothing -> do
                      mbUser <- dbQuery $ LoginUserCreds creds
                      case mbUser of
                        Just user -> do
                          modifySession (sessionUserID .~ Just (user ^. userID))
                          Spock.redirect "/"
                        Nothing -> errorNoAssociatedUser
                Nothing -> errorNoExternalUser
        checkCallbackCsrf successAction = do
          csrf <- getCsrfToken
          clientCsrf <- param' csrfTokenParam
          if clientCsrf == csrf
          then successAction
          else errorInvalidCsrf
        csrfTokenParam = "state"
        errorInvalidCsrf = abort "Broken/invalid CSRF token (Guide error)"
        errorInvalidToken = abort "Invalid code for external provider"
        errorNoExternalUser = abort "No external user"
        errorNoAssociatedUser = abort "No user associated with external account"
        abort error = do
          setStatus status403
          Spock.text error
    Nothing -> return ()

fetchGithubProfile :: Manager -> AccessToken -> IO (Maybe Creds)
fetchGithubProfile manager token = do
  userResult <- authGetJSON manager token "https://api.github.com/user"
  mailResult <- authGetJSON manager token "https://api.github.com/user/emails"

  case (userResult, mailResult) of
    (Right _, Right []) -> return Nothing -- throwIO $ InvalidProfileResponse "github" "no mail address for user"
    (Right user, Right mails) -> return . Just $ toCreds user mails token
    (Left _err, _) -> return $ Nothing -- throwIO $ InvalidProfileResponse "github" err
    (_, Left _err) -> return $ Nothing -- throwIO $ InvalidProfileResponse "github" err

toCreds :: GithubUser -> [GithubUserEmail] -> AccessToken -> Creds
toCreds user userMails token = makeCreds "github" ident extra
    where
      ident = T.pack $ show $ githubUserId user
      extra = Map.fromList $

        Nothing
        `maybeCons`
        [
        ("email", githubUserEmailAddress email),
        ("login", githubUserLogin user),
        ("avatar_url", githubUserAvatarUrl user),
        ("access_token", T.decodeUtf8 $ accessToken token) ]
      email = fromMaybe (head userMails) $ find githubUserEmailPrimary userMails

maybeCons :: Maybe a -> [a] -> [a]
maybeCons Nothing  as = as
maybeCons (Just a) as = a : as
