module Guide.Routes where

import Prelude

import Control.Alt ((<|>))
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Guide.Types (CategoryName(..))
import Guide.Utils (Uid(..))
import Pux.Router (end, lit, router, str)

data Route
    = Home
    | CategoryOverview CategoryName
    | CategoryDetail CategoryName (Uid String) -- String == (Uid Category)
    -- TODO: Use `Uid Category` instead of `Uid String`
    -- if we have found a way to bridge `Uid a` properly from `Haskell` to `PS`
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
  CategoryOverview <<< CategoryName <$> (lit categoryLit *> str) <* end
  <|>
  CategoryDetail <<< CategoryName <$> (lit categoryLit *> str)
                                  <*> ((\s -> Uid {uidToText : s}) <$> str) <* end
  <|>
  Playground <$ (lit playgroundLit) <* end

toUrl :: Route -> String
toUrl (NotFound url) = url
toUrl Home = homeUrl
toUrl (CategoryOverview catName) = categoryUrl catName
toUrl (CategoryDetail catName catId) = categoryDetailUrl catName catId
toUrl Playground = playgroundUrl

litUrl :: String -> String
litUrl lit = "/" <> lit

homeUrl :: String
homeUrl = "/"

categoryLit :: String
categoryLit = "category"

categoryUrl :: CategoryName -> String
categoryUrl (CategoryName name) = (litUrl categoryLit) <> (litUrl name)

categoryDetailLit :: String
categoryDetailLit = "detail"

categoryDetailUrl :: CategoryName -> Uid String -> String
categoryDetailUrl catName (Uid catId) = (categoryUrl catName) <> (litUrl catId.uidToText)

playgroundLit :: String
playgroundLit = "playground"

playgroundUrl :: String
playgroundUrl = litUrl playgroundLit
