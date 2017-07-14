module Guide.Http where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error, error)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Aeson (options)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Either (Either(..), either)
import Data.Generic (class Generic)
import Guide.Types (CGrandCategories, CategoryName(..), Users)
import Lib.IsomorphicFetch (FETCH, fetch)

apiEndpoint :: String
apiEndpoint = "http://localhost:3080/"

decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson options

decodeJson' :: forall a. Generic a => Json -> Either Error a
decodeJson' = either (Left <<< error) pure <<< decodeJson

fetchUsers :: forall eff. Aff (fetch :: FETCH | eff) (Either String Users)
fetchUsers = do
  res <- attempt $ fetch "https://jsonplaceholder.typicode.com/users"
  pure $ either (Left <<< show) decodeJson res

fetchGrandCategories :: forall eff. CategoryName -> Aff (fetch :: FETCH | eff) (Either String CGrandCategories)
fetchGrandCategories (CategoryName cName) = do
  res <- attempt <<< fetch $ apiEndpoint <> cName <> "/api/all-categories"
  pure $ either (Left <<< show) decodeJson res
