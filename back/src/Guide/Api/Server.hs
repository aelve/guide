{-# LANGUAGE FlexibleContexts  #-}

module Guide.Api.Server
(
  runApiServer,
)
where

import Imports

import Network.Wai (Middleware, Request)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), corsOrigins,
                                    simpleCorsResourcePolicy)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Swagger.UI

import Guide.Api.Guider
import Guide.Api.Methods
import Guide.Api.Types
import Guide.Api.Docs
import Guide.Logger
import Guide.Config
import Guide.State

import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Acid as Acid
import qualified Network.Wai.Middleware.Cors as Cors

-- | The type that 'runApiServer' serves.
type FullApi =
  Api :<|>
  SwaggerSchemaUI "api" "swagger.json"

-- | Serve the API on port 4400.
runApiServer :: Logger -> Config -> Acid.AcidState GlobalState -> IO ()
runApiServer logger Config{..} db = do
  logDebugIO logger $ format "API is running on port {}" portApi
  let guideSettings = Warp.defaultSettings
        & Warp.setOnException (logException logger)
        & Warp.setPort portApi
  Warp.runSettings guideSettings $ corsPolicy $
    serve (Proxy @FullApi) (fullServer db logger Config{..})
  where
    corsPolicy :: Middleware
    corsPolicy =
        if cors then Cors.cors (const $ Just policy)
        else Cors.cors (const Nothing)
    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
        -- TODO: Add Guide's frontend address (and maybe others resources)
        -- to list of `corsOrigins` to allow CORS requests
        { corsOrigins = Just (
          [ "http://localhost:3333" -- Guide's frontend running on localhost
          , toUtf8ByteString (format "http://localhost:{}" portApi :: Text) -- The /api endpoint
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

-- | Collect API and Swagger server to united 'FullApi'. First takes
-- precedence in case of overlap.
fullServer :: DB -> Logger -> Config -> Server FullApi
fullServer db di config = apiServer db di config :<|> docServer

-- | Collect api out of guiders and convert them to handlers. Type 'type
-- Server api = ServerT api Handler' needed it.
apiServer :: DB -> Logger -> Config -> Server Api
apiServer db di config = do
  requestDetails <- ask
  hoistServer (Proxy @Api) (guiderToHandler (Context config db requestDetails) di)
      (const $ toServant site)

-- | A 'Server' for Swagger docs.
docServer :: Server (SwaggerSchemaUI "api" "swagger.json")
docServer = swaggerSchemaUIServer apiSwaggerDoc

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
