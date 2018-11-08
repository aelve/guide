{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}


module Guide.Api.Server
  ( runApiServer
  )
  where


import Imports

import Data.Swagger.Lens
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

import Data.Acid as Acid
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

apiServer :: DB -> Site AsServer
apiServer db = Site
  { _categorySite = toServant (CategorySite
      { _getCategories    = getCategories db
      , _getCategory      = getCategory db
      , _createCategory   = createCategory db
      , _setCategoryNotes = setCategoryNotes db
      , _setCategoryInfo  = setCategoryInfo db
      , _deleteCategory   = deleteCategory db }
      :: CategorySite AsServer)

  , _itemSite = toServant (ItemSite
      { _createItem     = createItem db
      , _setItemInfo    = setItemInfo db
      , _deleteItem     = deleteItem db }
      :: ItemSite AsServer)

  , _traitSite = toServant (TraitSite
      { _createTrait    = createTrait db
      , _setTrait       = setTrait db
      , _deleteTrait    = deleteTrait db }
      :: TraitSite AsServer)

  , _searchSite = toServant (SearchSite
      { _search         = search db }
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
  say $ T.concat ["API is running on port ", toText $ show _portApi]
  run _portApi $ corsPolicy Config{..} $ serve (Proxy @FullApi) (fullServer db)
  where
    corsPolicy :: Config -> Middleware
    corsPolicy Config{..} =
        if _cors then cors (const $ Just (policy _portApi))
        else cors (const Nothing)
    policy :: Int -> CorsResourcePolicy
    policy portApi = simpleCorsResourcePolicy
        -- TODO: Add Guide's frontend address (and maybe others resources)
        -- to list of `corsOrigins` to allow CORS requests
        { corsOrigins = Just ([
              "http://localhost:3333" -- Guide's frontend running on localhost
            , BSC.concat ["http://localhost:", toByteString $ show portApi]  -- The /api endpoint
            ], True)
        }
