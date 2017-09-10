module Guide.Common.Types where

import Prelude

import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)
import Guide.Api.Types (CategoryInfo)
import IsomorphicFetch (FETCH)

type AppEffects eff = (fetch :: FETCH | eff)

newtype CategoryName = CategoryName String
derive instance gCategoryName :: Generic CategoryName
derive instance newtypeCategoryName :: Newtype CategoryName _
instance showCategoryName :: Show CategoryName where
  show = gShow

type CCategories = Array CategoryInfo
