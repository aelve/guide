module Guide.CategoryDetail.Events where

import Prelude

import Pux (EffModel, noEffects)

import Guide.CategoryDetail.Routes (Route)
import Guide.CategoryDetail.State (State(..))
import Guide.Common.Types (AppEffects)


data Event = PageView Route

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
