{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Guide.Api.Types
  ( Api
  , ApiError(..)
  , CategoryInfo(..)
  , CCategoryDetail(..)
  , CUid(..)
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
  {
  -- | A list of all categories (the /haskell page). Returns category
  -- titles.
    _getCategories :: route :-
      "categories"    :> Get '[JSON] (Either ApiError [CategoryInfo])

  -- | Details of a single category (and items in it, etc)
  , _getCategory :: route :-
      "category"      :> Capture "id" (Uid Category)
                      :> Get '[JSON] (Either ApiError CCategoryDetail)
  }
  deriving (Generic)

type Api = ToServant (Site AsApi)
----------------------------------------------------------------------------
-- Client types
----------------------------------------------------------------------------

data ApiError = ApiError !Text
  deriving (Generic)

instance A.FromJSON ApiError
instance A.ToJSON ApiError

data CategoryInfo = CategoryInfo
  { categoryInfoUid     :: CUid String
  , categoryInfoTitle   :: Text
  , categoryInfoCreated :: UTCTime
  , categoryInfoGroup_  :: Text
  , categoryInfoStatus  :: CategoryStatus
  }
  deriving (Show, Generic)

instance A.ToJSON CategoryInfo

-- | Client type of `Category`, which describes a category info
toCategoryInfo :: Category -> CategoryInfo
toCategoryInfo Category{..} = CategoryInfo
  { categoryInfoUid     = toCUid _categoryUid
  , categoryInfoTitle   = _categoryTitle
  , categoryInfoCreated = _categoryCreated
  , categoryInfoGroup_  = _categoryGroup_
  , categoryInfoStatus  = _categoryStatus
  }

-- | Client type of `Category`, which describes a category detail
data CCategoryDetail = CCategoryDetail
  { ccdUid :: CUid String
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
toCCategoryDetail Category{..} = CCategoryDetail
  { ccdUid = toCUid _categoryUid
  , ccdTitle = _categoryTitle
  , ccdGroup = _categoryGroup_
  , ccdDescription = toCMarkdown _categoryNotes
  , ccdItems = fmap toCItem _categoryItems
  , ccdStatus = _categoryStatus
  }

-- | Client type of `Item`
data CItem = CItem
  { ciUid :: CUid String
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
toCItem Item{..} = CItem
  { ciUid = toCUid _itemUid
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
  { ctUid :: CUid String
  , ctContent :: CMarkdown
  } deriving (Show, Generic)


instance A.ToJSON CTrait where
  toJSON = A.genericToJSON A.defaultOptions

toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctUid = toCUid (trait ^. uid)
  , ctContent = toCMarkdown $ trait ^. content
  }

-- | Client type of `Markdown`
data CMarkdown = CMarkdown
  { text :: Text
  , html :: Text
  } deriving (Show, Generic)

instance A.ToJSON CMarkdown where
  toJSON = A.genericToJSON A.defaultOptions

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

-- | Client type of `Uid`.
-- It's needed because we don't find a way
-- to bridge `Uid a` properly (for example `Uid Category` )
newtype CUid a = CUid Text
  deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance A.ToJSON (CUid a) where
  toJSON = A.genericToJSON A.defaultOptions

toCUid :: Uid a -> CUid b
toCUid (Uid t) = CUid t
