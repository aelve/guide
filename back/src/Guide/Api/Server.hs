{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
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

import Guide.Api.Guider (Context (..), GuiderServer, guiderToHandler)
import Guide.Api.Methods
import Guide.Api.Types
import Guide.Config (Config (..))
import Guide.State

import Data.Acid as Acid
import qualified Data.ByteString.Char8 as BSC

guiderServer :: Site GuiderServer
guiderServer = Site
  { _categorySite = toServant (CategorySite
      { _getCategories    = getCategories
      , _getCategory      = getCategory
      , _createCategory   = createCategory
      , _setCategoryNotes = setCategoryNotes
      , _setCategoryInfo  = setCategoryInfo
      , _deleteCategory   = deleteCategory }
      :: CategorySite GuiderServer)

  , _itemSite = toServant (ItemSite
      { _getItem          = getItem
      , _createItem       = createItem
      , _setItemInfo      = setItemInfo
      , _setItemSummary   = setItemSummary
      , _setItemEcosystem = setItemEcosystem
      , _setItemNotes     = setItemNotes
      , _deleteItem       = deleteItem
      , _moveItem         = moveItem }
      :: ItemSite GuiderServer)

  , _traitSite = toServant (TraitSite
      { _createTrait = createTrait
      , _setTrait    = setTrait
      , _deleteTrait = deleteTrait
      , _moveTrait   = moveTrait }
      :: TraitSite GuiderServer)

  , _searchSite = toServant (SearchSite
      { _search = search }
      :: SearchSite GuiderServer)
  }

type FullApi =
  Api :<|>
  SwaggerSchemaUI "api" "swagger.json"

fullServer :: DB -> Config -> Server FullApi
fullServer db config =
  api db config :<|>
  swaggerSchemaUIServer doc
  where
    doc = toSwagger (Proxy @Api)
            & info.title   .~ "Aelve Guide API"
            & info.version .~ "alpha"

-- | 'hoistServer' brings custom type server to 'Handler' type server. Custem types not consumed by servant.
api :: DB -> Config -> Server Api
api db config = do
  requestDetails <- ask
  hoistServer (Proxy @Api) (guiderToHandler (Context config db requestDetails))
      (const $ toServant guiderServer)

-- | Serve the API on port 4400.
--
-- You can test this API by doing @withDB mempty runApiServer@.
runApiServer :: Config -> AcidState GlobalState -> IO ()
runApiServer Config{..} db = do
  say $ format "API is running on port {}" _portApi
  run _portApi $ corsPolicy $ serve (Proxy @FullApi) (fullServer db Config{..})
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
