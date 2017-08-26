module Guide.Client.CategoryDetail where

import Prelude
import Guide.CategoryDetail.Events (AppEffects, Event(..), foldp)
import Guide.CategoryDetail.Routes (match)
import Guide.CategoryDetail.State (State, init)
import Guide.CategoryDetail.View.Layout (view)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [routeSignal] }

  -- | Render to the DOM
  renderToDOM "#guide" app.markup app.input

  -- | Return Guide.Client.CategoryDetail.to be used for hot reloading logic in support/client.entry.js
  pure app

initialState :: State
initialState = init "/"
