{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Guide.Api.Types
  ( Api
  , CCategoryInfo(..)
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

import Guide.Api.Error
import Guide.Api.Utils
import Guide.Types.Core
         ( Category(..), CategoryStatus(..), Item(..), ItemKind
         , Trait, content, uid )
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
      :> Get '[JSON] [CCategoryInfo]

  , _getCategory :: route :-
      Summary "Get details of a category, and its full contents"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "id" (Uid Category)
      :> Get '[JSON] CCategoryDetail
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

-- | A "light-weight" client type of `Category`, which describes a category info
data CCategoryInfo = CCategoryInfo
  { cciUid     :: Uid Category          <?> "Category ID"
  , cciTitle   :: Text                  <?> "Title"
  , cciCreated :: UTCTime               <?> "When the category was created"
  , cciGroup_  :: Text                  <?> "Category group ('grandcategory')"
  , cciStatus  :: CategoryStatus        <?> "Status (done, in progress, ...)"
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryInfo where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CCategoryInfo where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a `CCategoryInfo` from a `Category`
toCategoryInfo :: Category -> CCategoryInfo
toCategoryInfo Category{..} = CCategoryInfo
  { cciUid     = H _categoryUid
  , cciTitle   = H _categoryTitle
  , cciCreated = H _categoryCreated
  , cciGroup_  = H _categoryGroup_
  , cciStatus  = H _categoryStatus
  }

-- | A "light-weight" client type of `Category`, which describes a category detail
data CCategoryDetail = CCategoryDetail
  { ccdUid :: Uid Category              <?> "Category ID"
  , ccdTitle :: Text                    <?> "Title"
  , ccdGroup :: Text                    <?> "Category group ('grandcategory')"
  , ccdStatus :: CategoryStatus         <?> "Status (done, in progress, ...)"
  , ccdDescription :: CMarkdown         <?> "Category description/notes"
  , ccdItems :: [CItem]                 <?> "All items in the category"
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryDetail where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CCategoryDetail where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a `CCategoryDetail` from a `Category`
toCCategoryDetail :: Category -> CCategoryDetail
toCCategoryDetail Category{..} = CCategoryDetail
  { ccdUid         = H $ _categoryUid
  , ccdTitle       = H $ _categoryTitle
  , ccdGroup       = H $ _categoryGroup_
  , ccdDescription = H $ toCMarkdown _categoryNotes
  , ccdItems       = H $ fmap toCItem _categoryItems
  , ccdStatus      = H $ _categoryStatus
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
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CItem where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

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
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a `CTrait` from a `Trait`
toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctUid = trait ^. uid
  , ctContent = toCMarkdown $ trait ^. content
  }

-- | Client type of `Markdown`
data CMarkdown = CMarkdown
  { text :: Text <?> "Markdown source"
  , html :: Text <?> "Rendered HTML"
  } deriving (Show, Generic)

instance A.ToJSON CMarkdown
instance ToSchema CMarkdown

-- | Type class to create `CMarkdown`
class ToCMardown md where toCMarkdown :: md -> CMarkdown

instance ToCMardown MarkdownInline where
  toCMarkdown md = CMarkdown
    { text = H $ md^.mdSource
    , html = H $ T.decodeUtf8 $ md^.mdHtml
    }

instance ToCMardown MarkdownBlock where
  toCMarkdown md = CMarkdown
    { text = H $ md^.mdSource
    , html = H $ T.decodeUtf8 $ md^.mdHtml
    }

instance ToCMardown MarkdownTree where
  toCMarkdown md = CMarkdown
    { text = H $ md^.mdSource
    , html = H $ T.toStrict . renderText $ toHtml md
    }

----------------------------------------------------------------------------
-- Schema instances
----------------------------------------------------------------------------

instance ToParamSchema (Uid a) where
  toParamSchema _ = mempty
    & S.type_ .~ SwaggerString
    & S.format ?~ "Text-based ID"

instance ToSchema (Uid a) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Uid") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema CategoryStatus

instance ToSchema ItemKind where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted schemaOptions
