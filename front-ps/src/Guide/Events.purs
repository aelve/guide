module Guide.Events where

import Guide.Api.ClientTypes (CCategoryDetail, CCategoryOverview)
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
  | RequestCategory CCategoryOverview
  | ReceiveCategory (Either String CCategoryDetail)
  -- playground
  | RequestUsers
  | ReceiveUsers (Either String Users)
