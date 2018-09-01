{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Guide.Api.Types
  (
  -- * API
    Api
  , Site(..)
  , CategorySite(..)
  , ItemSite(..)
  , TraitSite(..)

  -- * View types
  , CCategoryInfo(..)
  , CCategoryDetail(..)
  , CItem(..)
  , CMarkdown(..)
  , CTrait(..)
  , toCCategoryDetail
  , toCategoryInfo
  )
  where


import Imports

import qualified Data.Aeson as A
import Lucid (toHtml, renderText)
import Servant
import Servant.API.Generic
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
  { _categorySite :: route :-
      BranchTag "Categories" "Working with categories."
      :> ToServant CategorySite AsApi
  , _itemSite :: route :-
      BranchTag "Items" "Working with items."
      :> ToServant ItemSite AsApi
  , _traitSite :: route :-
      BranchTag "Item traits" "Working with item traits."
      :> ToServant TraitSite AsApi
  }
  deriving (Generic)

data CategorySite route = CategorySite
  { _getCategories :: route :-
      Summary "Get a list of available categories"
      :> "categories"
      :> Get '[JSON] [CCategoryInfo]

  , _getCategory :: route :-
      Summary "Get full contents of a category"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "id" (Uid Category)
      :> Get '[JSON] CCategoryDetail

  , _createCategory :: route :-
      Summary "Create a new category"
      :> Description "Returns the ID of the created category.\n\n\
                     \If a category with the same title already exists, \
                     \returns its ID instead."
      :> "category"
      :> QueryParam' '[Required, Strict] "title" Text
      :> Post '[JSON] (Uid Category)

  , _deleteCategory :: route :-
      Summary "Delete a category"
      :> Description "Note: please ignore the 404 here, it's a bug \
                     \in documentation. This endpoint does not return 404."
      :> "category"
      :> Capture "id" (Uid Category)
      :> Delete '[JSON] NoContent
  }
  deriving (Generic)

data ItemSite route = ItemSite
  { _createItem :: route :-
      Summary "Create a new item in the given category"
      :> "item"
      :> Capture "category" (Uid Category)
      :> QueryParam' '[Required, Strict] "name" Text
      :> Post '[JSON] (Uid Item)

  , _deleteItem :: route :-
      Summary "Delete an item"
      :> Description "Note: please ignore the 404 here, it's a bug \
                     \in documentation. This endpoint does not return 404."
      :> "item"
      :> Capture "id" (Uid Item)
      :> Delete '[JSON] NoContent
  }
  deriving (Generic)

data TraitSite route = TraitSite
  { _deleteTrait :: route :-
      Summary "Delete a trait"
      :> Description "Note: please ignore the 404 here, it's a bug \
                     \in documentation. This endpoint does not return 404."
      :> "item"
      :> Capture "item" (Uid Item)
      :> "trait"
      :> Capture "id" (Uid Trait)
      :> Delete '[JSON] NoContent
  }
  deriving (Generic)

type Api = ToServant Site AsApi

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
    , html = H $ toText $ md^.mdHtml
    }

instance ToCMardown MarkdownBlock where
  toCMarkdown md = CMarkdown
    { text = H $ md^.mdSource
    , html = H $ toText $ md^.mdHtml
    }

instance ToCMardown MarkdownTree where
  toCMarkdown md = CMarkdown
    { text = H $ md^.mdSource
    , html = H $ toText . renderText $ toHtml md
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
  declareNamedSchema _ = pure $ NamedSchema (Just "CategoryID") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema (Uid Item) where
  declareNamedSchema _ = pure $ NamedSchema (Just "ItemID") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema (Uid Trait) where
  declareNamedSchema _ = pure $ NamedSchema (Just "TraitID") $ mempty
    & S.type_ .~ SwaggerString

instance ToSchema CategoryStatus

instance ToSchema ItemKind where
  declareNamedSchema _ = pure $ NamedSchema (Just "ItemKind") $ mempty
    & S.type_ .~ SwaggerObject
    & S.format ?~ "Can be one of the three things:\
                  \ {tag: Library, contents: <package name>}\
                  \ * {tag: Tool, contents: <package name>}\
                  \ * {tag: Other}"
