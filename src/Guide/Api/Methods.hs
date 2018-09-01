{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Guide.Api.Methods
  ( getCategories
  , getCategory
  )
  where


import Imports

import Servant
import Data.Acid as Acid

import Guide.Types
import Guide.State
import Guide.Utils (Uid)
import Guide.Api.Types (CategoryInfo, CCategoryDetail, toCategoryInfo, toCCategoryDetail)

getCategories :: DB -> Handler [CategoryInfo]
getCategories db = do
  liftIO (Acid.query db GetCategories) <&> \xs ->
    map toCategoryInfo xs

getCategory :: DB -> Uid Category -> Handler CCategoryDetail
getCategory db catId =
  liftIO (Acid.query db (GetCategoryMaybe catId)) >>= \case
    Nothing  -> throwError err404
    Just cat -> pure (toCCategoryDetail cat)
