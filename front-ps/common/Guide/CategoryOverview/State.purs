module Guide.CategoryOverview.State where

import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Guide.Common.Api (ApiError)
import Guide.CategoryOverview.Routes (Route, match)
import Guide.Common.Types (CategoryName(..), CCategories)
import Network.RemoteData (RemoteData(..))

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , errors :: Array String
  , categories :: RemoteData ApiError CCategories
  , categoryName :: CategoryName
  }

derive instance gState :: Generic State
derive instance newtypeState :: Newtype State _
instance showState :: Show State where
  show = gShow

init :: String -> State
init url = State
  { title: "CategoryOverview page" -- TODO (sectore): Change title
  , route: match url
  , loaded: false
  , errors: []
  , categories: NotAsked
  , categoryName: CategoryName catNameHS
  }

catNameHS :: String
catNameHS = "haskell"
