{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Guide.Api.Types
  ( Api
  , ApiError(..)
  , CategoryInfo(..)
  , CCategoryDetail(..)
  , CItem(..)
  , CMarkdown(..)
  , CTrait(..)
  , Site(..)
  , toCCategoryDetail
  , toCategoryInfo
  )
  where


import Imports

import qualified Data.Aeson as A
import qualified Data.Text.All as T
import Lucid (toHtml, renderText)
import Servant
import Servant.Generic
import Data.Swagger as S

import Guide.Types.Core (Category(..), CategoryStatus(..), Item(..), ItemKind
  , Trait, content, uid
  )
import Guide.Utils (Uid(..), Url)
import Guide.Markdown (MarkdownBlock, MarkdownInline, MarkdownTree, mdHtml, mdSource)

----------------------------------------------------------------------------
-- Routes
----------------------------------------------------------------------------

-- | The description of the served API.
data Site route = Site
  { _getCategories :: route :-
      Summary "Get all categories"
      :> "categories"
      :> Get '[JSON] (Either ApiError [CategoryInfo])

  , _getCategory :: route :-
      Summary "Get details of a category, and its full contents"
      :> "category"
      :> Capture "id" (Uid Category)
      :> Get '[JSON] (Either ApiError CCategoryDetail)
  }
  deriving (Generic)

type Api = ToServant (Site AsApi)

----------------------------------------------------------------------------
-- Client types
--
-- These are more "light-weight" Haskell types of `Guide`.
--
-- Furthermore using these "light-weight" types we keep all data small
-- to send these over the wire w/o having deep nested data,
-- we might not need on front-end.
----------------------------------------------------------------------------

-- | Client-side API error
newtype ApiError = ApiError Text
  deriving (Generic)

instance A.FromJSON ApiError
instance A.ToJSON ApiError
instance ToSchema ApiError

-- | A "light-weight" client type of `Category`, which describes a category info
data CategoryInfo = CategoryInfo
  { categoryInfoUid     :: Uid Category
  , categoryInfoTitle   :: Text
  , categoryInfoCreated :: UTCTime
  , categoryInfoGroup_  :: Text
  , categoryInfoStatus  :: CategoryStatus
  }
  deriving (Show, Generic)

instance A.ToJSON CategoryInfo
instance ToSchema CategoryInfo

-- | Factory to create a `CategoryInfo` from a `Category`
toCategoryInfo :: Category -> CategoryInfo
toCategoryInfo Category{..} = CategoryInfo
  { categoryInfoUid     = _categoryUid
  , categoryInfoTitle   = _categoryTitle
  , categoryInfoCreated = _categoryCreated
  , categoryInfoGroup_  = _categoryGroup_
  , categoryInfoStatus  = _categoryStatus
  }

-- | A "light-weight" client type of `Category`, which describes a category detail
data CCategoryDetail = CCategoryDetail
  { ccdUid :: Uid Category
  , ccdTitle :: Text
  , ccdGroup :: Text
  , ccdDescription :: CMarkdown
  , ccdItems :: [CItem]
  , ccdStatus :: CategoryStatus
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryDetail where
  toJSON = A.genericToJSON A.defaultOptions

instance ToSchema CCategoryDetail

-- | Factory to create a `CCategoryDetail` from a `Category`
toCCategoryDetail :: Category -> CCategoryDetail
toCCategoryDetail Category{..} = CCategoryDetail
  { ccdUid = _categoryUid
  , ccdTitle = _categoryTitle
  , ccdGroup = _categoryGroup_
  , ccdDescription = toCMarkdown _categoryNotes
  , ccdItems = fmap toCItem _categoryItems
  , ccdStatus = _categoryStatus
  }

-- | Client type of `Item`
data CItem = CItem
  { ciUid :: Uid Item
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

instance ToSchema CItem

-- | Factory to create a `CItem` from an `Item`
toCItem :: Item -> CItem
toCItem Item{..} = CItem
  { ciUid = _itemUid
  , ciName = _itemName
  , ciCreated = _itemCreated
  , ciGroup = _itemGroup_
  , ciDescription = toCMarkdown _itemDescription
  , ciPros = fmap toCTrait _itemPros
  , ciProsDeleted = fmap toCTrait _itemProsDeleted
  , ciCons = fmap toCTrait _itemCons
  , ciConsDeleted = fmap toCTrait _itemConsDeleted
  , ciEcosystem = toCMarkdown _itemEcosystem
  , ciNotes = toCMarkdown _itemNotes
  , ciLink = _itemLink
  , ciKind = _itemKind
  }

-- | Client type of `Trait`
data CTrait = CTrait
  { ctUid :: Uid Trait
  , ctContent :: CMarkdown
  } deriving (Show, Generic)

instance A.ToJSON CTrait where
  toJSON = A.genericToJSON A.defaultOptions

instance ToSchema CTrait

-- | Factory to create a `CTrait` from a `Trait`
toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctUid = trait ^. uid
  , ctContent = toCMarkdown $ trait ^. content
  }

-- | Client type of `Markdown`
data CMarkdown = CMarkdown
  { text :: Text
  , html :: Text
  } deriving (Show, Generic)

instance A.ToJSON CMarkdown where
  toJSON = A.genericToJSON A.defaultOptions

instance ToSchema CMarkdown

-- | Type class to create `CMarkdown`
class ToCMardown md where toCMarkdown :: md -> CMarkdown

instance ToCMardown MarkdownInline where
  toCMarkdown md = CMarkdown
    { text = md^.mdSource
    , html = T.decodeUtf8 $ md^.mdHtml
    }

instance ToCMardown MarkdownBlock where
  toCMarkdown md = CMarkdown
    { text = md^.mdSource
    , html = T.decodeUtf8 $ md^.mdHtml
    }

instance ToCMardown MarkdownTree where
  toCMarkdown md = CMarkdown
    { text = md^.mdSource
    , html = T.toStrict . renderText $ toHtml md
    }

----------------------------------------------------------------------------
-- Schema instances
----------------------------------------------------------------------------

instance ToParamSchema (Uid a) where
  toParamSchema _ = mempty
    & S.type_ .~ SwaggerString
    & S.format ?~ "Text-based ID"

instance ToSchema (Uid a)

instance ToSchema CategoryStatus

instance ToSchema ItemKind where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
