module Guide.Common.Routes where

import Prelude

import Guide.Api.Types (CUid(..))
import Guide.Common.Types (CategoryName(..))

litUrl :: String -> String
litUrl lit = "/" <> lit

categoryLit :: String
categoryLit = "category"

categoryUrl :: CategoryName -> String
categoryUrl (CategoryName name) = (litUrl categoryLit) <> (litUrl name)

categoryDetailUrl :: forall a. CategoryName -> CUid a -> String
categoryDetailUrl catName (CUid catId) = (categoryUrl catName) <> (litUrl catId)
