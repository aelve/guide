module Guide.CategoryDetail.Routes where

import Data.Function (($))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router)

data Route = Home | NotFound String

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/"
