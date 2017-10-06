module Guide.Common.Api where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut.Generic.Aeson (options)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, unsafeFromForeign)
import Data.Generic (class Generic)
import Guide.Api.Types (CategoryInfo, CCategoryDetail)
import Guide.Utils (Uid)
import Guide.Common.Types (CCategories, CategoryName, unwrapUid)
import IsomorphicFetch (FETCH, get, json)

endpoint :: String
endpoint = "http://localhost:4400"

data EndpointError
    = JSONDecodingError String
    | ServerError String

derive instance gEndpointError :: Generic EndpointError

instance showEndpointError :: Show EndpointError where
  show (JSONDecodingError e) =
      "[JSONDecodingError]: " <> show e
  show (ServerError e) =
      "[ServerError]: " <> show e

-- | Decoder for json data
decodeJson :: forall a. (Generic a) => Foreign -> Either String a
decodeJson = genericDecodeJson options <<< unsafeFromForeign

-- | Decodes a result considering JSON and Server errors
decodeResult :: forall a. Generic a => Foreign -> Either EndpointError a
decodeResult = either (Left <<< JSONDecodingError) (bimap ServerError id) <<< decodeJson

-- | Fetches all categories
getCategories :: forall eff. CategoryName -> Aff (fetch :: FETCH | eff) (Either EndpointError CCategories)
getCategories _ = do
  response <- get $ endpoint <> "/categories"
  json' <- json response
  pure $ decodeResult json'

-- | Fetches a categories by a given category id
getCategory :: forall eff. CategoryName -> (Uid CategoryInfo) -> Aff (fetch :: FETCH | eff) (Either EndpointError CCategoryDetail)
getCategory _ catId = do
  response <- get $ endpoint <> "/category/" <> unwrapUid catId
  json' <- json response
  pure $ decodeResult json'
