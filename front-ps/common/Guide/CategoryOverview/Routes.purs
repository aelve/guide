module Guide.CategoryOverview.Routes where

import Prelude

import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Pux.Router (end, lit, router, str)
import Guide.Common.Routes (categoryLit, categoryUrl)
import Guide.Common.Types (CategoryName(..))

data Route
  = CategoryOverview CategoryName
  | NotFound String

derive instance genericRoute :: Generic Route
instance showRoute :: Show Route where
    show = gShow
instance eqRoute :: Eq Route where
    eq = gEq

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  CategoryOverview <<< CategoryName <$> (lit categoryLit *> str) <* end

toURL :: Route -> String
toURL (NotFound url)              = url
toURL (CategoryOverview catName)  = categoryUrl catName
