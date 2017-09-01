module Guide.CategoryDetail.State where

import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show (class Show)

import Guide.Api.Types (CCategoryDetail)
import Guide.CategoryDetail.Routes (Route, match)

newtype State = State
  { title :: String
  , route :: Route
  , category :: Maybe CCategoryDetail
  , loaded :: Boolean
  }

derive instance gState :: Generic State
derive instance newtypeState :: Newtype State _
instance showState :: Show State where
  show = gShow

init :: String -> State
init url = State
  { title: "CategoryDetail page" -- TODO (sectore): Change title
  , route: match url
  , category: Nothing
  , loaded: false
  }
