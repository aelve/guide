module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, message)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Node.Express.App (App, get, listenHttp, setProp, use, useOnError)
import Node.Express.Handler (Handler, HandlerM, next)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getOriginalUrl, getRouteParam)
import Node.Express.Response (redirect, render, sendJson, setStatus)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)

errorHandler :: forall e. Error -> HandlerM (express :: EXPRESS | e) Unit
errorHandler err = do
  setStatus 400
  sendJson {error: message err}

logger :: forall e. Handler (console :: CONSOLE | e)
logger = do
  url   <- getOriginalUrl
  liftEff $ log ("url: " <> url)
  next

indexHandler :: forall e. HandlerM (express :: EXPRESS | e ) Unit
indexHandler =
  redirect $ "/category/" <> defaultCategoryName

defaultCategoryName :: String
defaultCategoryName = "haskell"

newtype PageConfig = PageConfig
  { contentId :: String
  , title :: String
  , catName :: String --
  , catDetailId :: String
  }

renderPage :: forall eff . PageConfig -> Handler eff
renderPage config =
  -- TODO (sectore) Render a single Pux app for each page
  render "layout" config

categoryOverviewHandler :: forall e. HandlerM (express :: EXPRESS | e) Unit
categoryOverviewHandler = do
  catName <- fromMaybe defaultCategoryName <$> getRouteParam "name"
  renderPage $ PageConfig { contentId: "category-overview"
                          , title: "Aelve - Guide: Category " <> catName
                          , catName
                          , catDetailId: "0"
                          }

categoryDetailHandler :: forall e. HandlerM (express :: EXPRESS | e) Unit
categoryDetailHandler = do
  catName <- fromMaybe defaultCategoryName <$> getRouteParam "name"
  catDetailId <- fromMaybe "0" <$> getRouteParam "id"
  renderPage $ PageConfig { contentId: "category-detail"
                          , title: "Aelve - Guide: Category Detail" <> catDetailId
                          , catName
                          , catDetailId
                          }

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
  liftEff $ log "app setup"

  setProp "view engine"   "ejs"
  setProp "views"         "public/views"

  use logger
  use $ static "public"

  get "/"                            indexHandler
  get "/category/:name/"             categoryOverviewHandler
  get "/category/:name/detail/:id"   categoryDetailHandler

  useOnError errorHandler

main :: forall e. Eff (express :: EXPRESS, process :: PROCESS, console :: CONSOLE | e) Server
main = do
  port <- (fromMaybe defaultPort <<< fromString <<< fromMaybe (show defaultPort)) <$> lookupEnv "PORT"
  listenHttp appSetup port (callback port)
  where
    defaultPort = 8080
    callback port' = \_ -> log $ "Listening on " <> show port'
