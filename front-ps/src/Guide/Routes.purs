module Guide.Routes where

import Prelude

import Control.Alt ((<|>))
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Pux.Router (end, lit, router)

data Route
    = Home
    | Haskell
    | Playground
    | NotFound String

derive instance genericRoute :: Generic Route
instance showRoute :: Show Route where
    show = gShow
instance eqRoute :: Eq Route where
    eq = gEq

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  Haskell <$ (lit haskellLit) <* end
  <|>
  Playground <$ (lit playgroundLit) <* end

litUrl :: String -> String
litUrl lit = "/" <> lit

toUrl :: Route -> String
toUrl (NotFound url) = url
toUrl Home = homeUrl
toUrl Haskell = haskellUrl
toUrl Playground = playgroundUrl

homeUrl :: String
homeUrl = "/"

haskellLit :: String
haskellLit = "haskell"

haskellUrl :: String
haskellUrl = litUrl haskellLit

playgroundLit :: String
playgroundLit = "playground"

playgroundUrl :: String
playgroundUrl = litUrl playgroundLit
