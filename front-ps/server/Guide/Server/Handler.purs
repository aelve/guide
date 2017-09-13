module Guide.Server.Handler where

import Prelude

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, message)
import Data.Argonaut.Generic.Aeson (options)
import Data.Argonaut.Generic.Encode (genericEncodeJson)
import Guide.Common.Types (AppEffects)
import Guide.CategoryOverview.Events (Event(..), foldp) as CO
import Guide.CategoryOverview.Routes (Route(..), match) as CO
import Guide.CategoryOverview.State (State(..), init) as CO
import Guide.CategoryOverview.View.Layout (view) as CO
import Guide.CategoryDetail.Events (Event(..), foldp) as CD
import Guide.CategoryDetail.Routes (Route(..), match) as CD
import Guide.CategoryDetail.State (State(..), init) as CD
import Guide.CategoryDetail.View.Layout (view) as CD
import Guide.Server.Constants (defaultCategoryName)
import Guide.Server.HTMLWrapper (htmlWrapper)
import Node.Express.Handler (HandlerM, Handler)
import Node.Express.Request (getOriginalUrl)
import Node.Express.Response (redirect, send, sendJson, setStatus)
import Node.Express.Types (EXPRESS)
import Pux (CoreEffects, start, waitState)
import Pux.Renderer.React (renderToStaticMarkup, renderToString)
import Signal (constant)

indexHandler :: forall e. HandlerM (express :: EXPRESS | e ) Unit
indexHandler =
  redirect $ "/category/" <> defaultCategoryName

errorHandler :: forall e. Error -> HandlerM (express :: EXPRESS | e) Unit
errorHandler err = do
  setStatus 500
  sendJson {error: message err}

categoryDetailHandler :: forall e. Handler (CoreEffects (AppEffects e))
categoryDetailHandler = do
  let getState (CD.State st) = st

  url <- getOriginalUrl

  app <- liftEff $ start
   { initialState: CD.init url
   , view: CD.view
   , foldp: CD.foldp
   , inputs: [constant (CD.PageView (CD.match url))]
   }

  state <- liftAff $ waitState (\(CD.State st) -> st.loaded) app

  case (getState state).route of
    (CD.NotFound _) -> setStatus 404
    _ -> setStatus 200

  html <- liftEff do
    let state_json = "window.__puxInitialState = "
                     <> (show $ genericEncodeJson options state)
                     <> ";"

    app_html <- renderToString app.markup
    renderToStaticMarkup $ constant (htmlWrapper app_html state_json "category-detail")

  send html


categoryOverviewHandler :: forall e. Handler (CoreEffects (AppEffects e))
categoryOverviewHandler = do
  let getState (CO.State st) = st

  url <- getOriginalUrl

  app <- liftEff $ start
   { initialState: CO.init url
   , view: CO.view
   , foldp: CO.foldp
   , inputs: [constant (CO.PageView (CO.match url))]
   }

  state <- liftAff $ waitState (\(CO.State st) -> st.loaded) app

  case (getState state).route of
    (CO.NotFound _) -> setStatus 404
    _ -> setStatus 200

  html <- liftEff do
    let state_json = "window.__puxInitialState = "
                     <> (show $ genericEncodeJson options state)
                     <> ";"

    app_html <- renderToString app.markup
    renderToStaticMarkup $ constant (htmlWrapper app_html state_json "category-overview")

  send html
