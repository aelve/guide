module Guide.Events where

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
  -- playground
  | RequestUsers
  | ReceiveUsers (Either String Users)
