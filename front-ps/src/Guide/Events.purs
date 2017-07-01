module Guide.Events where

import Prelude

import Data.Maybe (Maybe(..))
import Guide.Routes (Route(..), match)
import Guide.State (State(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)

data Event
  -- Routing
  = PageView Route
  | Navigate String DOMEvent

type AppEffects eff = (ajax :: AJAX | eff)

foldp :: ∀ eff. Event -> State -> EffModel State Event (AppEffects eff)
foldp (Navigate url ev) state = onlyEffects state
  [ -- TODO (sectore): Update history (on client side only)
  pure <<< Just $ PageView (match url)
  ]

foldp (PageView route) (State st) =
  routeEffects route (State $ st { route = route, loaded = true })

routeEffects :: ∀ fx. Route -> State -> EffModel State Event (AppEffects fx)
routeEffects Home s@(State st) = noEffects $ s
routeEffects Haskell s@(State st) = noEffects $ s
routeEffects Playground s@(State st) = noEffects $ s
routeEffects (NotFound url) s@(State st) = noEffects $ s
