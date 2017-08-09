module Guide.Events where

import Guide.Api.ClientTypes (CCategoryDetail, CUid)
import Data.Either (Either)
import Guide.Routes (Route)
import Guide.Types (CGrandCategories, CategoryName, Users)
import Pux.DOM.Events (DOMEvent)

data Event
  -- Routing
  = PageView Route
  | Navigate String DOMEvent
  -- API
  | RequestGrandCategories CategoryName
  | ReceiveGrandCategories (Either String CGrandCategories)
  | RequestCategory CategoryName (CUid String)
  -- TODO: ^ Use `CUid Category` instead of `CUid String` as second type parameter
  -- if we have found a way to bridge `Uid a` properly from `Haskell` to `PS`
  | ReceiveCategory (Either String CCategoryDetail)
  -- playground
  | RequestUsers
  | ReceiveUsers (Either String Users)
