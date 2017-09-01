{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}


module Guide.Api
(
  Site(..),
  runApiServer,
)
where


import Imports

import Data.Aeson
import Servant
import Servant.Generic
import Network.Wai.Handler.Warp (run)
import Data.Acid as Acid
-- putStrLn that works well with concurrency
import Say (say)

import Guide.Types
import Guide.State
import Guide.Utils (Uid)


----------------------------------------------------------------------------
-- Routes
----------------------------------------------------------------------------

-- | The description of the served API.
data Site route = Site
  {
  -- | A list of all categories (the /haskell page). Returns category
  -- titles.
    _getCategories :: route :-
      "categories"              :> Get '[JSON] [Text]

  -- | Details of a single category (and items in it, etc)
  , _getCategory :: route :-
      "category"                :> Capture "id" (Uid Category)
                                :> Get '[JSON] (Either ApiError Category)
  }
  deriving (Generic)

data ApiError = ApiError !Text
  deriving (Generic)

instance FromJSON ApiError
instance ToJSON ApiError

----------------------------------------------------------------------------
-- Boilerplate
----------------------------------------------------------------------------

apiServer :: DB -> Site AsServer
apiServer db = Site {
  _getCategories = getCategories db,
  _getCategory   = getCategory db
  }

type Api = ToServant (Site AsApi)

-- | Serve the API on port 4400.
--
-- You can test this API by doing @withDB mempty runApiServer@.
runApiServer :: AcidState GlobalState -> IO ()
runApiServer db = do
  say "API is running on port 4400"
  run 4400 $ serve (Proxy @Api) (toServant (apiServer db))

----------------------------------------------------------------------------
-- Implementations of methods
----------------------------------------------------------------------------

getCategories :: DB -> Handler [Text]
getCategories db = do
  liftIO (Acid.query db GetCategories) <&> \xs ->
    map (view title) xs

getCategory :: DB -> Uid Category -> Handler (Either ApiError Category)
getCategory db catId =
  liftIO (Acid.query db (GetCategoryMaybe catId)) <&> \case
    Nothing  -> Left (ApiError "category not found")
    Just cat -> Right cat
