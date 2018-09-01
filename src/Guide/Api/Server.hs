{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}


module Guide.Api.Server
  ( runApiServer
  )
  where


import Imports

import Data.Acid as Acid
import Servant
import Servant.Generic
import Servant.Swagger
import Servant.Swagger.UI.ReDoc
import Network.Wai.Handler.Warp (run)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors
  , corsOrigins, simpleCorsResourcePolicy)

-- putStrLn that works well with concurrency
import Say (say)

import Guide.State
import Guide.Api.Types (Api, Site(..))
import Guide.Api.Methods (getCategories, getCategory)

apiServer :: DB -> Site AsServer
apiServer db = Site
  { _getCategories = getCategories db
  , _getCategory   = getCategory db
  }

type FullApi =
  Api :<|>
  SwaggerSchemaUI "api" "swagger.json"

fullServer :: DB -> Server FullApi
fullServer db =
  toServant (apiServer db) :<|>
  redocSchemaUIServer (toSwagger (Proxy @Api))

-- | Serve the API on port 4400.
--
-- You can test this API by doing @withDB mempty runApiServer@.
runApiServer :: AcidState GlobalState -> IO ()
runApiServer db = do
  say "API is running on port 4400"
  run 4400 $ corsPolicy $ serve (Proxy @FullApi) (fullServer db)
  where
    corsPolicy :: Middleware
    corsPolicy = cors (const $ Just policy)
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
                -- TODO: Add Guide's frontend address (and maybe others resources)
                -- to list of `corsOrigins` to allow CORS requests
                { corsOrigins = Just ([ "http://localhost:3333"
                                      -- ^ Guide's frontend running on localhost
                                      ], True)
                }
