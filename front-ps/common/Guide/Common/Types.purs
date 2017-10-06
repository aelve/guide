module Guide.Common.Types where

import Prelude

import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)
import Guide.Api.Types (CategoryInfo)
import Guide.Utils (Uid(..))
import IsomorphicFetch (FETCH)

type AppEffects eff = (fetch :: FETCH | eff)

newtype CategoryName = CategoryName String
derive instance gCategoryName :: Generic CategoryName
derive instance newtypeCategoryName :: Newtype CategoryName _
instance showCategoryName :: Show CategoryName where
  show = gShow

type CCategories = Array CategoryInfo

-- helper

mkUid :: forall a . String -> Uid a
mkUid s = Uid {uidToText: s}

unwrapUid :: forall a . Uid a -> String
unwrapUid (Uid uid) = uid.uidToText
