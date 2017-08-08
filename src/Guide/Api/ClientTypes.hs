{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
  This module describes API's client types. These are needed to generate
  Guide's Haskell types to PureScript types by `purescript-bridge`.
  All types will be provided as JSON to send and receive all data between
  back- and frontend.
-}

module Guide.Api.ClientTypes
  ( CCategoryDetail(..)
  , CCategoryOverview(..)
  , CGrandCategory(..)
  , CItem(..)
  , CMarkdown(..)
  , CTrait(..)
  , toCGrandCategory
  , toCCategoryDetail
  )
  where

import Imports

import qualified Data.Aeson as A
import qualified Data.Text.All as T
import Lucid (toHtml, renderText)

import Guide.Types.Core (Category, CategoryStatus(..), Item, ItemKind, Trait
  , content, created, group_, description, pros, prosDeleted, cons, consDeleted, ecosystem
  , notes, link, kind, name, items, notes, status, uid, title, group_
  )
import Guide.Utils (Uid(..), Url)
import Guide.Markdown (MarkdownBlock, MarkdownInline, MarkdownTree, mdHtml, mdText)
import Guide.Views.Utils (categoryLink)

-- | Client type of `Category`, which describes a category overview
data CCategoryOverview = CCategoryOverview
  { ccoUid    :: Uid String
  , ccoTitle  :: Text
  , ccoLink  :: Text
  } deriving (Show, Generic)

instance A.ToJSON CCategoryOverview where
  toJSON = A.genericToJSON A.defaultOptions

toCCategoryOverview :: Category -> CCategoryOverview
toCCategoryOverview cat = CCategoryOverview
  { ccoUid = toUid (cat^.uid)
  , ccoTitle = cat^.title
  , ccoLink = categoryLink cat
  }

-- | Client type of `Category`, which describes a top or grand category
data CGrandCategory = CGrandCategory
  { cgcTitle     :: Text
  , cgcFinished  :: [CCategoryOverview]
  , cgcWip       :: [CCategoryOverview]
  , cgcStub      :: [CCategoryOverview]
  }
  deriving (Show, Generic)

instance A.ToJSON CGrandCategory where
  toJSON = A.genericToJSON A.defaultOptions

toCGrandCategory :: [Category] -> CGrandCategory
toCGrandCategory cats = CGrandCategory
  { cgcTitle = cats^?!_head.group_
  , cgcFinished = fmap toCCategoryOverview (filter ((== CategoryFinished) . view status) cats)
  , cgcWip = fmap toCCategoryOverview (filter ((== CategoryWIP) . view status) cats)
  , cgcStub = fmap toCCategoryOverview (filter ((== CategoryStub) . view status) cats)
  }

-- | Client type of `Category`, which describes a category detail
data CCategoryDetail = CCategoryDetail
  { ccdUid :: Uid String
  , ccdTitle :: Text
  , ccdGroup :: Text
  , ccdDescription :: CMarkdown
  , ccdItems :: [CItem]
  , ccdStatus :: CategoryStatus
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryDetail where
  toJSON = A.genericToJSON A.defaultOptions

toCCategoryDetail :: Category -> CCategoryDetail
toCCategoryDetail cat = CCategoryDetail
  { ccdUid = toUid (cat^.uid)
  , ccdTitle = cat^.title
  , ccdGroup = cat^.group_
  , ccdDescription = toCMarkdown $ cat^.notes
  , ccdItems = fmap toCItem (cat^.items)
  , ccdStatus = cat^.status
  }

-- | Client type of `Item`
data CItem = CItem
  { ciUid :: Uid String
  , ciName :: Text
  , ciCreated :: UTCTime
  , ciGroup :: Maybe Text
  , ciDescription :: CMarkdown
  , ciPros :: [CTrait]
  , ciProsDeleted :: [CTrait]
  , ciCons :: [CTrait]
  , ciConsDeleted :: [CTrait]
  , ciEcosystem :: CMarkdown
  , ciNotes :: CMarkdown
  , ciLink :: Maybe Url
  , ciKind :: ItemKind
  } deriving (Show, Generic)

instance A.ToJSON CItem where
  toJSON = A.genericToJSON A.defaultOptions

toCItem :: Item -> CItem
toCItem item = CItem
  { ciUid = toUid (item ^. uid)
  , ciName = item ^. name
  , ciCreated = item ^. created
  , ciGroup = item ^. group_
  , ciDescription = toCMarkdown $ item ^. description
  , ciPros = fmap toCTrait (item ^. pros)
  , ciProsDeleted = fmap toCTrait (item ^. prosDeleted)
  , ciCons = fmap toCTrait (item ^. cons)
  , ciConsDeleted = fmap toCTrait (item ^. consDeleted)
  , ciEcosystem = toCMarkdown $ item ^. ecosystem
  , ciNotes = toCMarkdown $ item ^. notes
  , ciLink = item ^. link
  , ciKind = item ^. kind
  }

data CTrait = CTrait
  { ctUid :: Uid String
  , ctContent :: CMarkdown
  } deriving (Show, Generic)


instance A.ToJSON CTrait where
  toJSON = A.genericToJSON A.defaultOptions

toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctUid = toUid (trait ^. uid)
  , ctContent = toCMarkdown $ trait ^. content
  }

data CMarkdown = CMarkdown
  { text :: Text
  , html :: Text
  } deriving (Show, Generic)

instance A.ToJSON CMarkdown where
  toJSON = A.genericToJSON A.defaultOptions

class ToCMardown md where toCMarkdown :: md -> CMarkdown

instance ToCMardown MarkdownInline where
  toCMarkdown md = CMarkdown
    { text = md^.mdText
    , html = T.decodeUtf8 $ md^.mdHtml
    }

instance ToCMardown MarkdownBlock where
  toCMarkdown md = CMarkdown
    { text = md^.mdText
    , html = T.decodeUtf8 $ md^.mdHtml
    }

instance ToCMardown MarkdownTree where
  toCMarkdown md = CMarkdown
    { text = md^.mdText
    , html = T.toStrict . renderText $ toHtml md
    }

toUid :: Uid a -> Uid b
toUid (Uid t) = Uid t
