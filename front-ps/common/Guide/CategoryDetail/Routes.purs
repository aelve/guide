module Guide.CategoryDetail.Routes where

import Prelude

import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Guide.Api.Types (CUid(..))
import Guide.Common.Routes (categoryLit, categoryDetailUrl)
import Guide.Common.Types (CategoryName(..))
import Pux.Router (end, lit, router, str)

data Route
  = CategoryDetail CategoryName (CUid String)
  | NotFound String

derive instance genericRoute :: Generic Route
instance showRoute :: Show Route where
    show = gShow
instance eqRoute :: Eq Route where
    eq = gEq


match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  CategoryDetail <<< CategoryName <$> (lit categoryLit *> str)
                                  <*> (CUid <$> str) <* end

toURL :: Route -> String
toURL (NotFound url)                  = url
toURL (CategoryDetail catName catId)  = categoryDetailUrl catName catId
