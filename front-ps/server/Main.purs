module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Guide.Server.Handler (categoryDetailHandler, categoryOverviewHandler, indexHandler, errorHandler)
import Node.Express.App (App, get, listenHttp, setProp, use, useOnError)
import Node.Express.Handler (Handler, next)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getOriginalUrl)
import Node.Express.Types (EXPRESS)
import Node.HTTP (Server)
import Node.Process (PROCESS, lookupEnv)

logger :: forall e. Handler (console :: CONSOLE | e)
logger = do
  url   <- getOriginalUrl
  liftEff $ log ("url: " <> url)
  next

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
