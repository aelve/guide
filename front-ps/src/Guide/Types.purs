module Guide.Types where

import Prelude

import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)
import Guide.Api.ClientTypes (CGrandCategory)
import Lib.IsomorphicFetch (FETCH)
import Network.HTTP.Affjax (AJAX)

type AppEffects eff =
  ( ajax :: AJAX
  , fetch :: FETCH
  , dom :: DOM
  , history :: HISTORY
  | eff
  )

type CGrandCategories = Array CGrandCategory

newtype User = User
  { id :: Int
  , name :: String
  , email :: String
  }

derive instance gUser :: Generic User
derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User
instance showUser :: Show User where
  show = gShow

newtype Users = Users (Array User)
derive instance gUsers :: Generic Users
derive instance newtypeUsers :: Newtype Users _
instance showUsers :: Show Users where
  show = gShow

newtype CategoryName = CategoryName String
derive instance gCategoryName :: Generic CategoryName
derive instance newtypeCategoryName :: Newtype CategoryName _
instance showCategoryName :: Show CategoryName where
  show = gShow
