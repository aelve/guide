module Guide.CategoryDetail.Events where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Guide.Api.Types (CategoryInfo, CCategoryDetail)
import Guide.Utils (Uid)
import Guide.CategoryDetail.Routes (Route(..))
import Guide.CategoryDetail.State (State(..))
import Guide.Common.Api (EndpointError, getCategory)
import Guide.Common.Types (AppEffects, CategoryName)
import Network.RemoteData (RemoteData(..), isNotAsked)
import Pux (EffModel, noEffects)


data Event
  = PageView Route
  | RequestCategory CategoryName (Uid CategoryInfo)
  | ReceiveCategory (Either EndpointError CCategoryDetail)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)

foldp (RequestCategory catName catId) (State state) =
  { state:
      State $ state { category = Loading
                    }
  , effects:
    [ getCategory catName catId >>= pure <<< Just <<< ReceiveCategory
    ]
  }

foldp (ReceiveCategory (Right cat)) (State state) = noEffects $
  State $
    state { loaded = true
          , category = Success cat
          }

foldp (ReceiveCategory (Left error)) (State state) = noEffects $
  State $
    state { loaded = true
          , errors = (show error) : state.errors
          , category = Failure error
          }


foldp (PageView route) (State state) =
  routeEffects route (State $ state { route = route })

routeEffects :: forall eff. Route -> State -> EffModel State Event (AppEffects eff)
routeEffects (CategoryDetail catName catId) (State state) =
  { state:
      State $ state { loaded = false
                    }
  , effects: [
        -- fetch a category only once
        if isNotAsked state.category then
          pure <<< Just $ RequestCategory catName catId
        else
          pure Nothing
      ]
  }

routeEffects (NotFound url) (State state) = noEffects $
  State $ state { loaded = true }
