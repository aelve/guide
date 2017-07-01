module Guide.Routes where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Pux.Router (end, lit, router)

data Route
    = Home
    | Haskell
    | Playground
    | NotFound String

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where
    show = genericShow
instance eqRoute :: Eq Route where
    eq = genericEq

instance decodeRoute :: Decode Route where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoute :: Encode Route where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

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
