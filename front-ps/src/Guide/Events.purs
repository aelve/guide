module Guide.Events where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Guide.Http (fetchGithubUsers)
import Guide.Routes (Route(..), match)
import Guide.State (State(..))
import Lib.IsomorphicFetch (FETCH)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)

data Event
  -- Routing
  = PageView Route
  | Navigate String DOMEvent
  -- playground
  | RequestGithubUsers
  | ReceiveGithubUsers (Either Error String)

type AppEffects eff = (ajax :: AJAX, fetch :: FETCH | eff)

foldp :: ∀ eff. Event -> State -> EffModel State Event (AppEffects eff)

-- Playground

foldp RequestGithubUsers state = onlyEffects state
  [ attempt (fetchGithubUsers 4) >>= pure <<< Just <<< ReceiveGithubUsers
  ]

foldp (ReceiveGithubUsers (Right users)) (State st) = noEffects $
  State $
    st  { loaded = true
        , users = users
        }

foldp (ReceiveGithubUsers (Left error)) s@(State st) = noEffects $
  State $
    st  { loaded = true
        , errors = (show error) : st.errors
        }

-- Routing
foldp (Navigate url ev) state = onlyEffects state
  [ -- TODO (sectore): Update history (on client side only)
    pure <<< Just $ PageView (match url)
  ]

foldp (PageView route) (State st) =
  routeEffects route (State $ st { route = route })

routeEffects :: ∀ fx. Route -> State -> EffModel State Event (AppEffects fx)
routeEffects Home s@(State st) = noEffects $
  State $ st { loaded = true }

routeEffects Haskell s@(State st) = noEffects $
  State $ st { loaded = true }

routeEffects Playground s@(State st) =
  { state: State $ st { loaded = false }
  , effects: [
      attempt (fetchGithubUsers 2) >>= pure <<< Just <<< ReceiveGithubUsers
  ]}

routeEffects (NotFound url) s@(State st) = noEffects $
  State $ st { loaded = true }
