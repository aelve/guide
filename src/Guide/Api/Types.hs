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
  { cciUid     :: Uid Category          ? "Category ID"
  , cciTitle   :: Text                  ? "Category title"
  , cciCreated :: UTCTime               ? "When the category was created"
  , cciGroup_  :: Text                  ? "Category group ('grandcategory')"
  , cciStatus  :: CategoryStatus        ? "Status (done, in progress, ...)"
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
  { ccdUid :: Uid Category              ? "Category ID"
  , ccdTitle :: Text                    ? "Category title"
  , ccdGroup :: Text                    ? "Category group ('grandcategory')"
  , ccdStatus :: CategoryStatus         ? "Status, e.g. done, in progress, ..."
  , ccdDescription :: CMarkdown         ? "Category description/notes (Markdown)"
  , ccdItems :: [CItem]                 ? "All items in the category"
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
  { ciUid :: Uid Item                   ? "Item ID"
  , ciName :: Text                      ? "Item name"
  , ciCreated :: UTCTime                ? "When the item was created"
  , ciGroup :: Maybe Text               ? "Item group"
  , ciDescription :: CMarkdown          ? "Item summary (Markdown)"
  , ciPros :: [CTrait]                  ? "Pros (positive traits)"
  , ciCons :: [CTrait]                  ? "Cons (negative traits)"
  , ciEcosystem :: CMarkdown            ? "The ecosystem description (Markdown)"
  , ciNotes :: CMarkdown                ? "Notes (Markdown)"
  , ciLink :: Maybe Url                 ? "Link to the official site, if exists"
  , ciKind :: ItemKind                  ? "Item kind, e.g. library, ..."
  } deriving (Show, Generic)

instance A.ToJSON CItem where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CItem where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a `CItem` from an `Item`
toCItem :: Item -> CItem
toCItem Item{..} = CItem
  { ciUid         = H $ _itemUid
  , ciName        = H $ _itemName
  , ciCreated     = H $ _itemCreated
  , ciGroup       = H $ _itemGroup_
  , ciDescription = H $ toCMarkdown _itemDescription
  , ciPros        = H $ fmap toCTrait _itemPros
  , ciCons        = H $ fmap toCTrait _itemCons
  , ciEcosystem   = H $ toCMarkdown _itemEcosystem
  , ciNotes       = H $ toCMarkdown _itemNotes
  , ciLink        = H $ _itemLink
  , ciKind        = H $ _itemKind
  }

-- | Client type of `Trait`
data CTrait = CTrait
  { ctUid :: Uid Trait                  ? "Trait ID"
  , ctContent :: CMarkdown              ? "Trait text (Markdown)"
  } deriving (Show, Generic)

instance A.ToJSON CTrait where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a `CTrait` from a `Trait`
toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctUid     = H $ trait ^. uid
  , ctContent = H $ toCMarkdown $ trait ^. content
  }

-- | Client type of `Markdown`
data CMarkdown = CMarkdown
  { text :: Text                        ? "Markdown source"
  , html :: Text                        ? "Rendered HTML"
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

instance ToParamSchema (Uid Category) where
  toParamSchema _ = mempty
    & S.type_ .~ SwaggerString
    & S.format ?~ "Category ID"

instance ToParamSchema (Uid Item) where
  toParamSchema _ = mempty
    & S.type_ .~ SwaggerString
    & S.format ?~ "Item ID"

instance ToParamSchema (Uid Trait) where
  toParamSchema _ = mempty
    & S.type_ .~ SwaggerString
    & S.format ?~ "Trait ID"

instance ToSchema (Uid Category) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Uid Category") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema (Uid Item) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Uid Item") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema (Uid Trait) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Uid Trait") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema CategoryStatus

instance ToSchema ItemKind where
  declareNamedSchema _ = pure $ NamedSchema (Just "ItemKind") $ mempty
    & S.type_ .~ SwaggerObject
    & S.format ?~ "Can be one of the three things:\
                  \ {tag: Library, contents: <package name>}\
                  \ * {tag: Tool, contents: <package name>}\
                  \ * {tag: Other}"
