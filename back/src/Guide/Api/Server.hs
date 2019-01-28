{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Guide.Api.Server
  ( runApiServer
  )
  where


import Imports

import Data.Swagger.Lens hiding (format)
import Network.Wai (Middleware, Request)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnException)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors, corsOrigins,
                                    simpleCorsResourcePolicy)
import Servant
import Servant.API.Generic
import Servant.Swagger
import Servant.Swagger.UI

import Guide.Api.Guider (Context (..), GuiderServer, guiderToHandler, DefDi)
import Guide.Api.Methods
import Guide.Api.Types
import Guide.Logger
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
      { _getTrait    = getTrait    
      , _createTrait = createTrait
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

fullServer :: DB -> DefDi -> Config -> Server FullApi
fullServer db di config =
  api db di config :<|>
  swaggerSchemaUIServer doc
  where
    doc = toSwagger (Proxy @Api)
            & info.title   .~ "Aelve Guide API"
            & info.version .~ "alpha"

-- | 'hoistServer' brings custom type server to 'Handler' type server. Custom types not consumed by servant.
api :: DB -> DefDi -> Config -> Server Api
api db di config = do
  requestDetails <- ask
  hoistServer (Proxy @Api) (guiderToHandler (Context config db requestDetails) di)
      (const $ toServant guiderServer)

emptyOnException :: Maybe Request -> SomeException -> IO ()
emptyOnException _ _ = pure ()

-- | Serve the API on port 4400.
--
-- You can test this API by doing @withDB mempty runApiServer@.
runApiServer :: DefDi -> Config -> AcidState GlobalState -> IO ()
runApiServer di Config{..} db = do
  debugIO di $ format "API is running on port {}" _portApi
  let guideSettings = setOnException emptyOnException $ setPort _portApi defaultSettings
  runSettings guideSettings $ corsPolicy $ serve (Proxy @FullApi) (fullServer db di Config{..})
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
