module Guide.Types where

import Prelude

import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)

newtype CategoryName = CategoryName String
derive instance gCategoryName :: Generic CategoryName
derive instance newtypeCategoryName :: Newtype CategoryName _
instance showCategoryName :: Show CategoryName where
  show = gShow
