module Guide.Common.Api where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut.Generic.Aeson (options)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)
import Guide.Common.Types (CCategories, CategoryName)
import IsomorphicFetch (FETCH, get, json)

endpoint :: String
endpoint = "http://localhost:4400"

-- TODO (sectore): Provide more API errors
-- such as
-- data ApiError
--     = StatusError String
--     | JSONError String
--     | ServerError B.ApiError
newtype ApiError = ApiError String
derive instance gApiError :: Generic ApiError
derive instance ntApiError :: Newtype ApiError _
instance sApiError :: Show ApiError where
  show = gShow

decodeJson :: forall a. (Generic a) => Foreign -> Either String a
decodeJson = genericDecodeJson options <<< unsafeFromForeign

getCategories :: forall eff. CategoryName -> Aff (fetch :: FETCH | eff) (Either ApiError CCategories)
getCategories _ = do
  response <- get $ endpoint <> "/categories"
  json' <- json response
  pure $ either (Left <<< ApiError) pure $ decodeJson json'
