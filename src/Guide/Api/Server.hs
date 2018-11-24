{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Guide.Api.Server
  ( runApiServer
  )
  where


import Imports

import Data.Swagger.Lens hiding (format)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsOrigins,
                                    simpleCorsResourcePolicy)
import Servant
import Servant.API.Generic
import Servant.Swagger
import Servant.Swagger.UI

-- putStrLn that works well with concurrency
import Say (say)

import Guide.Api.Guider (GuiderServer, guiderToHandler)
import Guide.Api.Methods
import Guide.Api.Types
import Guide.Config (Config (..))
import Guide.State

import Data.Acid as Acid
import qualified Data.ByteString.Char8 as BSC

guiderServer :: DB -> Site GuiderServer
guiderServer db = Site
  { _categorySite = toServant (CategorySite
      { _getCategories    = getCategories db
      , _getCategory      = getCategory db
      , _createCategory   = createCategory db
      , _setCategoryNotes = setCategoryNotes db
      , _setCategoryInfo  = setCategoryInfo db
      , _deleteCategory   = deleteCategory db }
      :: CategorySite GuiderServer)

  , _itemSite = toServant (ItemSite
      { _createItem       = createItem db
      , _setItemInfo      = setItemInfo db
      , _setItemSummary   = setItemSummary db
      , _setItemEcosystem = setItemEcosystem db
      , _setItemNotes     = setItemNotes db
      , _deleteItem       = deleteItem db }
      :: ItemSite GuiderServer)

  , _traitSite = toServant (TraitSite
      { _createTrait    = createTrait db
      , _setTrait       = setTrait db
      , _deleteTrait    = deleteTrait db }
      :: TraitSite GuiderServer)

  , _searchSite = toServant (SearchSite
      { _search         = search db }
      :: SearchSite GuiderServer)
  }

type FullApi =
  Api :<|>
  SwaggerSchemaUI "api" "swagger.json"

fullServer :: DB -> Server FullApi
fullServer db =
  api db :<|>
  swaggerSchemaUIServer doc
  where
    doc = toSwagger (Proxy @Api)
            & info.title   .~ "Aelve Guide API"
            & info.version .~ "alpha"

-- | 'hoistServer' brings custom type server to 'Handler' type server. Custem types not consumed by servant.
api :: DB -> Server Api
api db = hoistServer (Proxy @Api) guiderToHandler (toServant (guiderServer db))

-- | Serve the API on port 4400.
--
-- You can test this API by doing @withDB mempty runApiServer@.
runApiServer :: Config -> AcidState GlobalState -> IO ()
runApiServer Config{..} db = do
  say $ format "API is running on port {}" _portApi
  run _portApi $ corsPolicy $ serve (Proxy @FullApi) (fullServer db)
  where
    corsPolicy :: Middleware
    corsPolicy =
        if _cors then cors (const $ Just (policy _portApi))
        else cors (const Nothing)
    policy :: Int -> CorsResourcePolicy
    policy portApi = simpleCorsResourcePolicy
        -- TODO: Add Guide's frontend address (and maybe others resources)
        -- to list of `corsOrigins` to allow CORS requests
        { corsOrigins = Just ([
              "http://localhost:3333" -- Guide's frontend running on localhost
            , BSC.pack $ format "http://localhost:{}" portApi  -- The /api endpoint
            ], True)
        }
