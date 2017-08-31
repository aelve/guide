module Guide.CategoryDetail.Routes where

import Prelude

import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Pux.Router (end, lit, router, str)
import Guide.Types (CategoryName(..))

data Route
  = CategoryDetail CategoryName String
  | NotFound String

derive instance genericRoute :: Generic Route
instance showRoute :: Show Route where
    show = gShow
instance eqRoute :: Eq Route where
    eq = gEq


match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  CategoryDetail <<< CategoryName <$> (lit categoryLit *> str)
                                  <*> str <* end

toURL :: Route -> String
toURL (NotFound url)                  = url
toURL (CategoryDetail catName catId)  = categoryDetailUrl catName catId

litUrl :: String -> String
litUrl lit = "/" <> lit

categoryLit :: String
categoryLit = "category"

categoryUrl :: CategoryName -> String
categoryUrl (CategoryName name) = (litUrl categoryLit) <> (litUrl name)

categoryDetailUrl :: CategoryName -> String -> String
categoryDetailUrl catName catId = (categoryUrl catName) <> (litUrl catId)
