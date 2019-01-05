{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Guide.Api.Types
  (
  -- * API
    Api
  , CategorySite(..)
  , ItemSite(..)
  , SearchSite(..)
  , Site(..)
  , TraitSite(..)

  -- * View types
  , CCategoryInfo(..), toCCategoryInfo
  , CCategoryInfoEdit(..)
  , CCategoryFull(..), toCCategoryFull
  , CItemInfo(..), toCItemInfo
  , CItemFull(..), toCItemFull
  , CMarkdown(..), toCMarkdown
  , CTrait(..), toCTrait

  -- * Search
  , CSearchResult(..), toCSearchResult

  -- * Other types
  , Move(..)
  , TraitType(..)
  , CTextEdit(..)
  , CMergeConflict(..)
  )
  where


import Imports

import Data.Tree (Forest, Tree)
import Lucid (renderText, toHtml)

import Servant
import Servant.API.Generic

import Guide.Api.Error
import Guide.Api.Utils
import Guide.Markdown
import Guide.Search
import Guide.Types.Core as G
import Guide.Utils (Uid (..), Url)

import qualified Data.Aeson as A
import Data.Swagger as S

----------------------------------------------------------------------------
-- Routes
----------------------------------------------------------------------------

-- | The description of the served API.
data Site route = Site
  { _categorySite :: route :-
      BranchTag "01. Categories" "Working with categories."
      :> ToServant CategorySite AsApi
  , _itemSite :: route :-
      BranchTag "02. Items" "Working with items."
      :> ToServant ItemSite AsApi
  , _traitSite :: route :-
      BranchTag "03. Item traits" "Working with item traits."
      :> ToServant TraitSite AsApi
  , _searchSite :: route :-
      BranchTag "04. Search" "Site-wide search."
      :> ToServant SearchSite AsApi
  }
  deriving (Generic)

-- | Working with categories
data CategorySite route = CategorySite
  { _getCategories :: route :-
      Summary "Get a list of available categories"
      :> Description "Primarily useful for displaying the main page. \
                     \The returned list is lightweight and doesn't contain \
                     \categories' contents."
      :> "categories"
      :> Get '[JSON] [CCategoryInfo]

  , _getCategory :: route :-
      Summary "Get contents of a category"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "id" (Uid Category)
      :> Get '[JSON] CCategoryFull

  , _createCategory :: route :-
      Summary "Create a new category"
      :> Description "Returns the ID of the created category.\n\n\
                     \If a category with the same title already exists \
                     \in the group, returns its ID instead."
      :> ErrorResponse 400 "'title' not provided"
      :> ErrorResponse 400 "'group' not provided"
      :> "category"
      :> QueryParam' '[Required, Strict,
                       Description "Title of the newly created category"]
           "title" Text
      :> QueryParam' '[Required, Strict,
                       Description "Group to put the category into"]
           "group" Text
      :> Post '[JSON] (Uid Category)

  , _setCategoryNotes :: route :-
      Summary "Edit category's notes"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "id" (Uid Category)
      :> "notes"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _setCategoryInfo :: route :-
      Summary "Set category's fields"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "id" (Uid Category)
      :> "info"
      :> ReqBody '[JSON] CCategoryInfoEdit
      :> Put '[JSON] NoContent

  , _deleteCategory :: route :-
      Summary "Delete a category"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "id" (Uid Category)
      :> Delete '[JSON] NoContent
  }
  deriving (Generic)

-- | Working with items
data ItemSite route = ItemSite
  { _createItem :: route :-
      Summary "Create a new item in the given category"
      :> Description "Returns the ID of the created item."
      :> ErrorResponse 400 "'name' not provided"
      :> "item"
      :> Capture "category" (Uid Category)
      :> QueryParam' '[Required, Strict] "name" Text
      :> Post '[JSON] (Uid Item)

  , _setItemInfo :: route :-
      Summary "Set item's info"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "info"
      :> ReqBody '[JSON] CItemInfo
      :> Put '[JSON] NoContent

  , _setItemSummary :: route :-
      Summary "Set item's summary"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "summary"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _setItemEcosystem :: route :-
      Summary "Set item's ecosystem"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "ecosystem"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _setItemNotes :: route :-
      Summary "Set item's notes"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "notes"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _deleteItem :: route :-
      Summary "Delete an item"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "id" (Uid Item)
      :> Delete '[JSON] NoContent

  , _moveItem :: route :-
      Summary "Move item"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "move"
      :> Capture "type" Move
      :> Put '[JSON] NoContent
  }
  deriving (Generic)

-- | Working with item traits
data TraitSite route = TraitSite
  { _createTrait :: route :-
      Summary "Create a new trait in the given item"
      :> Description "Returns the ID of the created trait."
      :> ErrorResponse 400 "'text' not provided"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "trait"
      :> Capture "type" TraitType
      :> ReqBody '[JSON] Text
      :> Post '[JSON] (Uid Trait)

  , _setTrait :: route :-
      Summary "Update a trait in the given item"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "trait"
      :> Capture "id" (Uid Trait)
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _deleteTrait :: route :-
      Summary "Delete a trait"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "trait"
      :> Capture "id" (Uid Trait)
      :> Delete '[JSON] NoContent

  , _moveTrait :: route :-
      Summary "Move trait"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "item" (Uid Item)
      :> "trait"
      :> Capture "id" (Uid Trait)
      :> "move"
      :> Capture "type" Move
      :> Put '[JSON] NoContent
  }
  deriving (Generic)

-- | Site-wide search
data SearchSite route = SearchSite
  { _search :: route :-
      Summary "Search categories and items"
      :> Description "Note: returns at most 100 search results."
      :> ErrorResponse 400 "'query' not provided"
      :> "search"
      :> QueryParam' '[Required, Strict] "query" Text
      :> Get '[JSON] [CSearchResult]
  }
  deriving (Generic)

type Api = RequestDetails :> ToServant Site AsApi

--------------------------------------------------------------------------
-- Additional types for routes
--------------------------------------------------------------------------

-- | Trait type (Pro/Con) and instances.
data TraitType = Pro | Con
    deriving (Show, Generic)

instance ToSchema TraitType where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance ToParamSchema TraitType where
    toParamSchema _ = mempty
        & S.type_  .~ SwaggerString
        & S.format ?~ "Trait type"

instance ToHttpApiData TraitType where
    toUrlPiece = toText . map toLower . show

instance FromHttpApiData TraitType where
    parseUrlPiece t = case t of
        "pro" -> Right Pro
        "con" -> Right Con
        _     -> Left "Invalid trait type!"

-- | Move (Upper/Lower) item or trait and their instances.
data Move = Upper | Lower -- 'Down' is busy.
    deriving (Eq, Show, Generic)

instance ToSchema Move where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance ToParamSchema Move where
    toParamSchema _ = mempty
        & S.type_  .~ SwaggerString
        & S.format ?~ "Move"

instance ToHttpApiData Move where
    toUrlPiece = toText . map toLower . show

instance FromHttpApiData Move where
    parseUrlPiece t = case t of
        "up"   -> Right Upper
        "down" -> Right Lower
        _      -> Left "Invalid move type!"

----------------------------------------------------------------------------
-- Client types
--
-- These are more "light-weight" Haskell types of 'Guide'.
--
-- Furthermore using these "light-weight" types we keep all data small
-- to send these over the wire w/o having deep nested data,
-- we might not need on front-end.
----------------------------------------------------------------------------

-- | A "light-weight" client type of 'Category', which describes a category
-- but doesn't give the notes or the items.
data CCategoryInfo = CCategoryInfo
  { cciUid     :: Uid Category   ? "Category ID"
  , cciTitle   :: Text           ? "Category title"
  , cciCreated :: UTCTime        ? "When the category was created"
  , cciGroup_  :: Text           ? "Category group ('grandcategory')"
  , cciStatus  :: CategoryStatus ? "Status (done, in progress, ...)"
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryInfo where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CCategoryInfo where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CCategoryInfo' from a 'Category'
toCCategoryInfo :: Category -> CCategoryInfo
toCCategoryInfo Category{..} = CCategoryInfo
  { cciUid     = H _categoryUid
  , cciTitle   = H _categoryTitle
  , cciCreated = H _categoryCreated
  , cciGroup_  = H _categoryGroup_
  , cciStatus  = H _categoryStatus
  }

-- | A "light-weight" client type of 'Category', which gives all available
-- information about a category
data CCategoryFull = CCategoryFull
  { ccfUid         :: Uid Category   ? "Category ID"
  , ccfTitle       :: Text           ? "Category title"
  , ccfGroup       :: Text           ? "Category group ('grandcategory')"
  , ccfStatus      :: CategoryStatus ? "Status, e.g. done, in progress, ..."
  , ccfDescription :: CMarkdown      ? "Category description/notes (Markdown)"
  , ccfItems       :: [CItemFull]    ? "All items in the category"
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryFull where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CCategoryFull where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CCategoryFull' from a 'Category'
toCCategoryFull :: Category -> CCategoryFull
toCCategoryFull Category{..} = CCategoryFull
  { ccfUid         = H $ _categoryUid
  , ccfTitle       = H $ _categoryTitle
  , ccfGroup       = H $ _categoryGroup_
  , ccfDescription = H $ toCMarkdown _categoryNotes
  , ccfItems       = H $ fmap toCItemFull _categoryItems
  , ccfStatus      = H $ _categoryStatus
  }

-- | Client type to edit meta category information.
data CCategoryInfoEdit = CCategoryInfoEdit
    { ccieTitle    :: Text            ? "Category title"
    , ccieGroup    :: Text            ? "Category group ('grandcategory')"
    , ccieStatus   :: CategoryStatus  ? "Status (done, in progress, ...)"
    , ccieSections :: Set ItemSection ? "Enabled item sections"
    }
    deriving (Show, Generic)

instance A.ToJSON CCategoryInfoEdit where
  toJSON = A.genericToJSON jsonOptions

instance A.FromJSON CCategoryInfoEdit where
  parseJSON = A.genericParseJSON jsonOptions

instance ToSchema CCategoryInfoEdit where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance ToSchema ItemSection where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | A lightweight info type about an 'Item'.
--
-- When updating it, don't forget to also update 'setItemInfo'.
data CItemInfo = CItemInfo
  { ciiUid     :: Uid Item   ? "Item ID"
  , ciiName    :: Text       ? "Item name"
  , ciiCreated :: UTCTime    ? "When the item was created"
  , ciiGroup   :: Maybe Text ? "Item group"
  , ciiHackage :: Maybe Text ? "Package name on Hackage"
  , ciiLink    :: Maybe Url  ? "Link to the official site, if exists"
  } deriving (Show, Generic)

instance A.ToJSON CItemInfo where
  toJSON = A.genericToJSON jsonOptions

instance A.FromJSON CItemInfo where
  parseJSON = A.genericParseJSON jsonOptions

instance ToSchema CItemInfo where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Client type of 'Item'
data CItemFull = CItemFull
  { cifUid         :: Uid Item                 ? "Item ID"
  , cifName        :: Text                     ? "Item name"
  , cifCreated     :: UTCTime                  ? "When the item was created"
  , cifGroup       :: Maybe Text               ? "Item group"
  , cifHackage     :: Maybe Text               ? "Package name on Hackage"
  , cifSummary     :: CMarkdown                ? "Item summary (Markdown)"
  , cifPros        :: [CTrait]                 ? "Pros (positive traits)"
  , cifCons        :: [CTrait]                 ? "Cons (negative traits)"
  , cifEcosystem   :: CMarkdown                ? "The ecosystem description (Markdown)"
  , cifNotes       :: CMarkdown                ? "Notes (Markdown)"
  , cifLink        :: Maybe Url                ? "Link to the official site, if exists"
  , cifToc         :: Forest CHeading          ? "Table of contents"
  } deriving (Show, Generic)

instance A.ToJSON CItemFull where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CItemFull where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CItemInfo' from an 'Item'
toCItemInfo :: Item -> CItemInfo
toCItemInfo Item{..} = CItemInfo
  { ciiUid         = H $ _itemUid
  , ciiName        = H $ _itemName
  , ciiCreated     = H $ _itemCreated
  , ciiGroup       = H $ _itemGroup_
  , ciiHackage     = H $ _itemHackage
  , ciiLink        = H $ _itemLink
  }

-- | Factory to create a 'CItemFull' from an 'Item'
toCItemFull :: Item -> CItemFull
toCItemFull Item{..} = CItemFull
  { cifUid         = H $ _itemUid
  , cifName        = H $ _itemName
  , cifCreated     = H $ _itemCreated
  , cifGroup       = H $ _itemGroup_
  , cifHackage     = H $ _itemHackage
  , cifSummary     = H $ toCMarkdown _itemSummary
  , cifPros        = H $ fmap toCTrait _itemPros
  , cifCons        = H $ fmap toCTrait _itemCons
  , cifEcosystem   = H $ toCMarkdown _itemEcosystem
  , cifNotes       = H $ toCMarkdown _itemNotes
  , cifLink        = H $ _itemLink
  , cifToc         = H $ map treeToCMD (markdownTreeMdTOC _itemNotes)
  }
  where
    treeToCMD = fmap toCHeading

-- | Client type of 'Trait'
data CTrait = CTrait
  { ctUid     :: Uid Trait ? "Trait ID"
  , ctContent :: CMarkdown ? "Trait text (Markdown)"
  } deriving (Show, Generic)

instance A.ToJSON CTrait where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CTrait' from a 'Trait'
toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctUid     = H $ trait ^. uid
  , ctContent = H $ toCMarkdown $ trait ^. content
  }

-- | Client type of 'Markdown'
data CMarkdown = CMarkdown
  { cmdText :: Text ? "Markdown source"
  , cmdHtml :: Text ? "Rendered HTML"
  } deriving (Show, Generic)

instance A.ToJSON CMarkdown where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CMarkdown where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Type class to create 'CMarkdown'
class ToCMarkdown md where toCMarkdown :: md -> CMarkdown

instance ToCMarkdown MarkdownInline where
  toCMarkdown md = CMarkdown
    { cmdText = H $ md^.mdSource
    , cmdHtml = H $ toText $ md^.mdHtml
    }

instance ToCMarkdown MarkdownBlock where
  toCMarkdown md = CMarkdown
    { cmdText = H $ md^.mdSource
    , cmdHtml = H $ toText $ md^.mdHtml
    }

instance ToCMarkdown MarkdownTree where
  toCMarkdown md = CMarkdown
    { cmdText = H $ md^.mdSource
    , cmdHtml = H $ toText . renderText $ toHtml md
    }

data CHeading = CHeading
  { chContent :: CMarkdown    ? "Rendered heading"
  , chSlug    :: Text         ? "In-page anchor for linking"
  } deriving (Show, Generic)

instance A.ToJSON CHeading where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CHeading where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

toCHeading :: Heading -> CHeading
toCHeading h = CHeading
  { chContent = H $ toCMarkdown $ headingMd h
  , chSlug    = H $ headingSlug h
  }

-- | Frontend sends this type to edit notes or descriptions.
data CTextEdit = CTextEdit
  { cteOriginal :: Text ? "State of base before editing"
  , cteModified :: Text ? "Modified text"
  } deriving (Show, Generic)

instance A.ToJSON CTextEdit where
  toJSON = A.genericToJSON jsonOptions

instance A.FromJSON CTextEdit where
  parseJSON = A.genericParseJSON jsonOptions

instance ToSchema CTextEdit where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Backend returns this type if there is conflict between state of base before and after editing.
data CMergeConflict = CMergeConflict
  { cmcOriginal       :: Text ? "State of base before editing"
  , cmcModified       :: Text ? "Modified text"
  , cmcServerModified :: Text ? "State of base after editing. (Base changed from another source)"
  , cmcMerged         :: Text ? "Merged text"
  } deriving (Eq, Show, Generic)

instance A.ToJSON CMergeConflict where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CMergeConflict where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

----------------------------------------------------------------------------
-- Search client types
----------------------------------------------------------------------------

-- | Client type of 'SearchResult'
data CSearchResult
  -- | Match was found in category title
  = CSRCategoryResult CSRCategory
  -- | Match was found in the item
  | CSRItemResult CSRItem
  deriving (Show, Generic)

instance A.ToJSON CSearchResult where
  toJSON = \case
    CSRCategoryResult cat -> A.object
      [ "tag" A..= ("Category" :: Text)
      , "contents" A..= cat
      ]
    CSRItemResult item -> A.object
      [ "tag" A..= ("Item" :: Text)
      , "contents" A..= item
      ]

instance ToSchema CSearchResult where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
    { constructorTagModifier = \case
        "CSRCategoryResult" -> "Category"
        "CSRItemResult" -> "Item"
        other -> error ("CSearchResult schema: unknown tag " <> show other)
    }
    & mapped.mapped.schema.S.description ?~
        "The docs lie. The true schema for this type is an object with two \
        \parameters 'tag' and 'contents', where 'tag' is one of keys listed \
        \in this doc, and 'contents' is the object."

-- | A category was found.
data CSRCategory = CSRCategory
  { csrcInfo        :: CCategoryInfo ? "Info about the category"
  , csrcDescription :: CMarkdown     ? "Category description"
  } deriving (Show, Generic)

instance A.ToJSON CSRCategory where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CSRCategory where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | An item was found.
data CSRItem = CSRItem
  { csriCategory    :: CCategoryInfo   ? "Category that the item belongs to"
  , csriInfo        :: CItemInfo       ? "Info about the item"
  , csriSummary     :: Maybe CMarkdown ? "Item summary (if the match was found there)"
  , csriEcosystem   :: Maybe CMarkdown ? "Item ecosystem (if the match was found there)"
  } deriving (Show, Generic)

instance A.ToJSON CSRItem where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CSRItem where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

toCSearchResult :: SearchResult -> CSearchResult
toCSearchResult (SRCategory cat) =
  CSRCategoryResult $ CSRCategory
    { csrcInfo        = H $ toCCategoryInfo cat
    , csrcDescription = H $ toCMarkdown $
        -- Extract the part before the first heading, to avoid showing the
        -- full description (we assume that the full description is too long
        -- and that the preface will accurately represent what the category
        -- is about).
        --
        -- TODO: just extract the first paragraph, not the preface.
        extractPreface $ toMarkdownTree "" $ cat^.G.notes.mdSource
    }
toCSearchResult (SRItem cat item) =
  CSRItemResult $ CSRItem
    { csriCategory    = H $ toCCategoryInfo cat
    , csriInfo        = H $ toCItemInfo item
    , csriSummary     = H $ Just (toCMarkdown (item ^. G.summary))
    , csriEcosystem   = H $ Nothing
    }
-- TODO: currently if there are matches in both item description and item
-- ecosystem, we'll show two matches instead of one
toCSearchResult (SRItemEcosystem cat item) =
  CSRItemResult $ CSRItem
    { csriCategory    = H $ toCCategoryInfo cat
    , csriInfo        = H $ toCItemInfo item
    , csriSummary     = H $ Nothing
    , csriEcosystem   = H $ Just (toCMarkdown (item ^. ecosystem))
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

instance ToSchema a => ToSchema (Tree a) where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions
