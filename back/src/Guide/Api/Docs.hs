-- | Rendered documentation for the API.
module Guide.Api.Docs
(
  apiSwaggerDoc,
  apiSwaggerRendered,
)
where

import Imports

import Servant.Swagger (toSwagger)
import Data.Swagger
import qualified Data.Aeson.Encode.Pretty as AesonPretty

import Guide.Api.Types

----------------------------------------------------------------------------
-- Swagger
----------------------------------------------------------------------------

-- | Swagger docs for the 'Api'.
apiSwaggerDoc :: Swagger
apiSwaggerDoc =
  toSwagger (Proxy @Api)
    & info.title   .~ "Aelve Guide API"
    & info.version .~ "alpha"

-- | Pretty-printed @swagger.json@ for the 'Api'.
apiSwaggerRendered :: Text
apiSwaggerRendered = utf8ToText $ AesonPretty.encodePretty apiSwaggerDoc
