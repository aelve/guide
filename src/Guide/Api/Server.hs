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
import Network.Wai.Handler.Warp (run)
-- putStrLn that works well with concurrency
import Say (say)

import Guide.State
import Guide.Api.Types (Api, Site(..))
import Guide.Api.Methods (getCategories, getCategory)

apiServer :: DB -> Site AsServer
apiServer db = Site {
  _getCategories = getCategories db,
  _getCategory   = getCategory db
  }

-- | Serve the API on port 4400.
--
-- You can test this API by doing @withDB mempty runApiServer@.
runApiServer :: AcidState GlobalState -> IO ()
runApiServer db = do
  say "API is running on port 4400"
  run 4400 $ serve (Proxy @Api) (toServant (apiServer db))
