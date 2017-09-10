module Guide.CategoryOverview.Events where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Guide.CategoryOverview.Routes (Route(..))
import Guide.CategoryOverview.State (State(..))
import Guide.Common.Api (EndpointError, getCategories)
import Guide.Common.Types (AppEffects, CategoryName, CCategories)
import Network.RemoteData (RemoteData(..), isNotAsked)
import Pux (EffModel, noEffects)

data Event =
  PageView Route
  -- API
  | GetCategories CategoryName
  | ReceiveCategories (Either EndpointError CCategories)

foldp :: forall eff. Event -> State -> EffModel State Event (AppEffects eff)

foldp (GetCategories catName) (State state) =
  { state: State $ state { categories = Loading
                         }
  , effects:
    [ getCategories catName >>= pure <<< Just <<< ReceiveCategories
    ]
  }

foldp (ReceiveCategories (Right cats)) (State st) = noEffects $
  State $
    st  { loaded = true
        , categories = Success cats
        }

foldp (ReceiveCategories (Left error)) s@(State st) = noEffects $
  State $
    st  { loaded = true
        , errors = (show error) : st.errors
        , categories = Failure error
        }

foldp (PageView route) (State st) =
  routeEffects route (State $ st { route = route })

routeEffects :: forall eff. Route -> State -> EffModel State Event (AppEffects eff)
routeEffects (CategoryOverview catName) s@(State st) =
  { state:
      State $ st  { loaded = false
                  , categoryName = catName
                  }
  , effects: [
      -- fetch grandCategories only once
      if isNotAsked st.categories then
        pure <<< Just $ GetCategories catName
      else
        pure Nothing
  ]}

routeEffects (NotFound url) s@(State st) = noEffects $
  State $ st { loaded = true }
