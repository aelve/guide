module Guide.CategoryDetail.State where

import Guide.CategoryDetail.Routes (Route, match)
import Data.Newtype (class Newtype)

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: "hello"
  , route: match url
  , loaded: false
  }
