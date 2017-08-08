module Guide.Http where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error, error)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Aeson (options)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Either (Either(..), either)
import Data.Generic (class Generic)
import Guide.Api.ClientTypes (CCategoryDetail)
import Guide.Types (CGrandCategories, CategoryName(..), Users)
import Guide.Utils (Uid(..))
import Lib.IsomorphicFetch (FETCH, fetch)

apiEndpoint :: String
apiEndpoint = "http://localhost:3080/api/"

decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson options

decodeJson' :: forall a. Generic a => Json -> Either Error a
decodeJson' = either (Left <<< error) pure <<< decodeJson

fetchUsers :: forall eff. Aff (fetch :: FETCH | eff) (Either String Users)
fetchUsers = do
  res <- attempt $ fetch "https://jsonplaceholder.typicode.com/users"
  pure $ either (Left <<< show) decodeJson res

fetchGrandCategories :: forall eff. CategoryName -> Aff (fetch :: FETCH | eff) (Either String CGrandCategories)
fetchGrandCategories (CategoryName catName) = do
  res <- attempt <<< fetch $ apiEndpoint <> catName <> "/all-categories"
  pure $ either (Left <<< show) decodeJson res

-- TODO: Use `Uid Category` instead of `String` as second function parameter
-- if we have found a way to bridge `Uid a` properly from `Haskell` to `PS`
fetchCategory :: forall eff. CategoryName -> (Uid String) -> Aff (fetch :: FETCH | eff) (Either String CCategoryDetail)
fetchCategory (CategoryName catName) (Uid catId) = do
  res <- attempt <<< fetch $ apiEndpoint <> catName <> "/category/" <> catId.uidToText
  pure $ either (Left <<< show) decodeJson res
