module Guide.Common.Routes where

import Prelude

import Guide.Api.Types (CategoryInfo)
import Guide.Utils (Uid(..))
import Guide.Common.Types (CategoryName(..))

litUrl :: String -> String
litUrl lit = "/" <> lit

categoryLit :: String
categoryLit = "category"

categoryUrl :: CategoryName -> String
categoryUrl (CategoryName name) = (litUrl categoryLit) <> (litUrl name)

categoryDetailUrl :: CategoryName -> Uid CategoryInfo -> String
categoryDetailUrl catName (Uid catId) = (categoryUrl catName) <> (litUrl catId.uidToText)
