module Guide.Server.Handler where

import Prelude

import Control.Monad.Eff.Exception (Error, message)
import Data.Maybe (fromMaybe)
import Guide.Server.Common (renderPage)
import Guide.Server.Constants (defaultCategoryName)
import Guide.Server.Types (PageConfig(..))
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (redirect, sendJson, setStatus)
import Node.Express.Types (EXPRESS)

indexHandler :: forall e. HandlerM (express :: EXPRESS | e ) Unit
indexHandler =
  redirect $ "/category/" <> defaultCategoryName

errorHandler :: forall e. Error -> HandlerM (express :: EXPRESS | e) Unit
errorHandler err = do
  setStatus 500
  sendJson {error: message err}

categoryDetailHandler :: forall e. HandlerM (express :: EXPRESS | e) Unit
categoryDetailHandler = do
  catName <- fromMaybe defaultCategoryName <$> getRouteParam "name"
  catDetailId <- fromMaybe "0" <$> getRouteParam "id"
  renderPage $ PageConfig { contentId: "category-detail"
                          , title: "Aelve - Guide: Category Detail" <> catDetailId
                          , catName
                          , catDetailId
                          }


categoryOverviewHandler :: forall e. HandlerM (express :: EXPRESS | e) Unit
categoryOverviewHandler = do
  catName <- fromMaybe defaultCategoryName <$> getRouteParam "name"
  renderPage $ PageConfig { contentId: "category-overview"
                          , title: "Aelve - Guide: Category " <> catName
                          , catName
                          , catDetailId: "0"
                          }
