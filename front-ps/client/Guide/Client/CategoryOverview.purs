module Guide.Client.CategoryOverview where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Data.Either (either)
import Data.Foreign (Foreign)
import Guide.Common.Api (decodeJson)
import Guide.CategoryOverview.Events (Event(..), foldp)
import Guide.CategoryOverview.Routes (match)
import Guide.CategoryOverview.State (State, init)
import Guide.CategoryOverview.View.Layout (view)
import Guide.Common.Types (AppEffects)
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM, console :: CONSOLE))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  urlSignal <- sampleURL =<< window
  let routeSignal = urlSignal ~> \r -> PageView (match r)
  app <- start  { initialState: state
                , view
                , foldp
                , inputs: [routeSignal]
                }
  renderToDOM "#guide" app.markup app.input
  log "Heeeelloooo, here is category-overview page rendered on client-side"
  pure app

-- | Used to serialize State from JSON in *-.entry.js
readState :: Foreign -> State
readState json = either (\_ -> init "/") id (decodeJson json)
