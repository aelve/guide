{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
  API's client types (e.g. generated to and used by PureScript)
-}
module Guide.Api.ClientTypes
  ( CCategoryDetail(..)
  , CCategoryOverview(..)
  , CGrandCategory(..)
  , mkCGrandCategory
  , mkCCategoryDetail
  )
  where

import Imports

import qualified Data.Aeson as A

import Guide.Types.Core (Category, CategoryStatus(..), Item, items, notes, status, uid, title, group_)
import Guide.Utils (Uid)
import Guide.Markdown (MarkdownBlock)
import Guide.Views.Utils (categoryLink)

data CCategoryOverview = CCategoryOverview
  { ccoUid    :: Uid Category
  , ccoTitle  :: Text
  , ccoLink  :: Text
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryOverview where
  toJSON = A.genericToJSON A.defaultOptions

mkCCategoryOverview :: Category -> CCategoryOverview
mkCCategoryOverview cat = CCategoryOverview
  { ccoUid = cat^.uid
  , ccoTitle = cat^.title
  , ccoLink = categoryLink cat
  }

data CGrandCategory = CGrandCategory
  { cgcTitle     :: Text
  , cgcFinished  :: [CCategoryOverview]
  , cgcWip       :: [CCategoryOverview]
  , cgcStub      :: [CCategoryOverview]
  }
  deriving (Show, Generic)

instance A.ToJSON CGrandCategory where
  toJSON = A.genericToJSON A.defaultOptions

mkCGrandCategory :: [Category] -> CGrandCategory
mkCGrandCategory cats = CGrandCategory
  { cgcTitle = cats^?!_head.group_
  , cgcFinished = fmap mkCCategoryOverview (filter ((== CategoryFinished) . view status) cats)
  , cgcWip = fmap mkCCategoryOverview (filter ((== CategoryWIP) . view status) cats)
  , cgcStub = fmap mkCCategoryOverview (filter ((== CategoryStub) . view status) cats)
  }

data CCategoryDetail = CCategoryDetail
  { ccdUid :: Uid Category
  , ccdTitle :: Text
  , ccdGroup :: Text
  , ccdDescription :: MarkdownBlock
  , ccdItems :: [Item]
  , ccdStatus :: CategoryStatus
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryDetail where
  toJSON = A.genericToJSON A.defaultOptions

mkCCategoryDetail :: Category -> CCategoryDetail
mkCCategoryDetail cat = CCategoryDetail
  { ccdUid = cat^.uid
  , ccdTitle = cat^.title
  , ccdGroup = cat^.group_
  , ccdDescription = cat^.notes
  , ccdItems = cat^.items
  , ccdStatus = cat^.status
  }
