module Guide.CategoryDetail.Routes where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Functor ((<$))
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Data.Show (class Show)
import Pux.Router (end, router)

data Route
  = Home
  | NotFound String

derive instance genericRoute :: Generic Route
instance showRoute :: Show Route where
    show = gShow
instance eqRoute :: Eq Route where
    eq = gEq

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/"
