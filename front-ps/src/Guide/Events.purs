module Guide.Events where

import Guide.Api.ClientTypes (CCategoryDetail)
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
  | RequestCategory CategoryName String -- (String == Uid Category)
  -- TODO: ^ Use `Uid Category` instead of `String` as second type parameter
  -- if we have found a way to bridge `Uid a` properly from `Haskell` to `PS`
  | ReceiveCategory (Either String CCategoryDetail)
  -- playground
  | RequestUsers
  | ReceiveUsers (Either String Users)
