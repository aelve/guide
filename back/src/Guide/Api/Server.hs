{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Guide.Api.Server
(
  runApiServer,
)
where

import Imports

import Data.Swagger.Lens hiding (format)
import Network.Wai (Middleware, Request)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsOrigins,
                                    simpleCorsResourcePolicy)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI

import Guide.Api.Guider
import Guide.Api.Methods
import Guide.Api.Types
import Guide.Logger
import Guide.Config
import Guide.State

import qualified Data.ByteString.Char8 as BSC
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Acid as Acid

-- | The type that 'runApiServer' serves.
type FullApi =
  Api :<|>
  SwaggerSchemaUI "api" "swagger.json"

-- | Serve the API on port 4400.
runApiServer :: Logger -> Config -> Acid.AcidState GlobalState -> IO ()
runApiServer logger Config{..} db = do
  logDebugIO logger $ format "API is running on port {}" _portApi
  let guideSettings = Warp.defaultSettings
        & Warp.setOnException (logException logger)
        & Warp.setPort _portApi
  Warp.runSettings guideSettings $ corsPolicy $
    serve (Proxy @FullApi) (fullServer db logger Config{..})
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

-- | An override for the default Warp exception handler.
--
-- Logs exceptions to the given 'Logger'.
logException :: Logger -> Maybe Request -> SomeException -> IO ()
logException logger mbReq ex =
  when (Warp.defaultShouldDisplayException ex) $
    logErrorIO logger $
      format "uncaught exception: {}; request info = {}" (show ex) (show mbReq)

----------------------------------------------------------------------------
-- Servant servers
----------------------------------------------------------------------------

fullServer :: DB -> Logger -> Config -> Server FullApi
fullServer db di config = apiServer db di config :<|> docServer

apiServer :: DB -> Logger -> Config -> Server Api
apiServer db di config = do
  requestDetails <- ask
  hoistServer (Proxy @Api) (guiderToHandler (Context config db requestDetails) di)
      (const $ toServant site)

-- | A 'Server' for Swagger docs.
docServer :: Server (SwaggerSchemaUI "api" "swagger.json")
docServer = swaggerSchemaUIServer doc
  where
    doc = toSwagger (Proxy @Api)
            & info.title   .~ "Aelve Guide API"
            & info.version .~ "alpha"

----------------------------------------------------------------------------
-- API handlers put together ('Site')
----------------------------------------------------------------------------

site :: Site (AsServerT Guider)
site = Site
  { _categorySite = toServant categorySite
  , _itemSite = toServant itemSite
  , _traitSite = toServant traitSite
  , _searchSite = toServant searchSite
  }

-- Individual branches

categorySite :: CategorySite (AsServerT Guider)
categorySite = CategorySite
  { _getCategories    = getCategories
  , _getCategory      = getCategory
  , _createCategory   = createCategory
  , _setCategoryNotes = setCategoryNotes
  , _setCategoryInfo  = setCategoryInfo
  , _deleteCategory   = deleteCategory
  }

itemSite :: ItemSite (AsServerT Guider)
itemSite = ItemSite
  { _getItem          = getItem
  , _createItem       = createItem
  , _setItemInfo      = setItemInfo
  , _setItemSummary   = setItemSummary
  , _setItemEcosystem = setItemEcosystem
  , _setItemNotes     = setItemNotes
  , _deleteItem       = deleteItem
  , _moveItem         = moveItem
  }

traitSite :: TraitSite (AsServerT Guider)
traitSite = TraitSite
  { _getTrait    = getTrait
  , _createTrait = createTrait
  , _setTrait    = setTrait
  , _deleteTrait = deleteTrait
  , _moveTrait   = moveTrait
  }

searchSite :: SearchSite (AsServerT Guider)
searchSite = SearchSite
  { _search = search
  }
