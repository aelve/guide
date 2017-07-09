module Guide.Events where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Guide.Http (fetchUsers, fetchGrandCategories)
import Guide.Routes (Route(..), match)
import Guide.State (State(..))
import Guide.Types (CGrandCategories, Users)
import Lib.IsomorphicFetch (FETCH)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..), isNotAsked)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)

data Event
  -- Routing
  = PageView Route
  | Navigate String DOMEvent
  -- API
  | RequestGrandCategories
  | ReceiveGrandCategories (Either String CGrandCategories)
  -- playground
  | RequestUsers
  | ReceiveUsers (Either String Users)

type AppEffects eff =
  ( ajax :: AJAX
  , fetch :: FETCH
  , dom :: DOM
  , history :: HISTORY
  | eff
  )

foldp :: ∀ eff. Event -> State -> EffModel State Event (AppEffects eff)

-- Api

foldp RequestGrandCategories (State st) =
  { state: State $ st { grandCategories = case st.grandCategories of
                                  Success cats -> Refreshing cats
                                  _ -> Loading
                      }
  , effects:
    [ fetchGrandCategories >>= pure <<< Just <<< ReceiveGrandCategories
    ]
  }

foldp (ReceiveGrandCategories (Right cats)) (State st) = noEffects $
  State $
    st  { loaded = true
        , grandCategories = (Success cats)
        }

foldp (ReceiveGrandCategories (Left error)) s@(State st) = noEffects $
  State $
    st  { loaded = true
        , errors = (show error) : st.errors
        , grandCategories = Failure error
        }

foldp RequestUsers (State st) =
  { state: State $ st { users = case st.users of
                                  Success users -> Refreshing users
                                  _ -> Loading
                      }
  , effects:
    [ fetchUsers >>= pure <<< Just <<< ReceiveUsers
    ]
  }

foldp (ReceiveUsers (Right users)) (State st) = noEffects $
  State $
    st  { loaded = true
        , users = (Success users)
        }

foldp (ReceiveUsers (Left error)) s@(State st) = noEffects $
  State $
    st  { loaded = true
        , errors = (show error) : st.errors
        , users = Failure error
        }

-- Routing
foldp (Navigate url ev) state = onlyEffects state
  -- TODO (sectore):
  -- Check if we can update history on client side only
  -- Currently we do have to declare `DOM` + `HISTORY` effects
  -- by handling `Navigate` Action in foldp
  [ do
      liftEff do
          preventDefault ev
          h <- history =<< window
          pushState (toForeign {}) (DocumentTitle "") (URL url) h
      pure <<< Just $ PageView (match url)
  ]

foldp (PageView route) (State st) =
  routeEffects route (State $ st { route = route })

routeEffects :: ∀ fx. Route -> State -> EffModel State Event (AppEffects fx)
routeEffects Home s@(State st) = noEffects $
  State $ st { loaded = true, countHomeRoute = st.countHomeRoute + 1 }

routeEffects Haskell s@(State st) =
  { state: State $ st { loaded = false }
  , effects: [
      -- fetch grandCategories only once
      if isNotAsked st.grandCategories then
        pure $ Just RequestGrandCategories
      else
        pure Nothing
  ]}

routeEffects Playground s@(State st) =
  { state: State $ st { loaded = false, countPGRoute = st.countPGRoute + 1 }
  , effects: [
      -- fetch users only once
      if isNotAsked st.users then
        pure $ Just RequestUsers
      else
        pure Nothing
  ]}

routeEffects (NotFound url) s@(State st) = noEffects $
  State $ st { loaded = true }
