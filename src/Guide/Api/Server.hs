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
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI

-- putStrLn that works well with concurrency
import Say (say)

import Guide.Api.Methods
import Guide.Api.Types
import Guide.Config (Config (..))
import Guide.State
import Guide.Api.Guider (guiderToHandler)

import Data.Acid as Acid
import qualified Data.ByteString.Char8 as BSC

apiServer :: DB -> Site AsServer
apiServer db = Site
  { _categorySite = toServant (CategorySite
      { _getCategories    = guiderToHandler $ getCategories db
      , _getCategory      = guiderToHandler . getCategory db
      , _createCategory   = (guiderToHandler .) . createCategory db
      , _setCategoryNotes = (guiderToHandler .) . setCategoryNotes db
      , _setCategoryInfo  = (guiderToHandler .) . setCategoryInfo db
      , _deleteCategory   = guiderToHandler . deleteCategory db }
      :: CategorySite AsServer)

  , _itemSite = toServant (ItemSite
      { _createItem       = (guiderToHandler .) . createItem db
      , _setItemInfo      = (guiderToHandler .) . setItemInfo db
      , _setItemSummary   = (guiderToHandler .) . setItemSummary db
      , _setItemEcosystem = (guiderToHandler .) . setItemEcosystem db
      , _setItemNotes     = (guiderToHandler .) . setItemNotes db
      , _deleteItem       = guiderToHandler . deleteItem db }
      :: ItemSite AsServer)

  , _traitSite = toServant (TraitSite
      { _createTrait    = ((guiderToHandler .) .) . createTrait db
      , _setTrait       = ((guiderToHandler .) .) . setTrait db
      , _deleteTrait    = (guiderToHandler .) . deleteTrait db }
      :: TraitSite AsServer)

  , _searchSite = toServant (SearchSite
      { _search         = guiderToHandler . search db }
      :: SearchSite AsServer)
  }

type FullApi =
  Api :<|>
  SwaggerSchemaUI "api" "swagger.json"

fullServer :: DB -> Server FullApi
fullServer db =
  toServant (apiServer db) :<|>
  swaggerSchemaUIServer doc
  where
    doc = toSwagger (Proxy @Api)
            & info.title   .~ "Aelve Guide API"
            & info.version .~ "alpha"

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
