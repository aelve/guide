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
  , CCategoryFull(..), toCCategoryFull
  , CItemInfo(..), toCItemInfo
  , CItemFull(..), toCItemFull
  , CMarkdown(..), toCMarkdown
  , CTrait(..), toCTrait

  -- * Request and response types
  , CCreateTrait(..)
  , CCategoryInfoEdit(..)
  , CItemInfoEdit(..)

  -- * Search
  , CSearchResult(..), toCSearchResult

  -- * Other types
  , CMove(..)
  , CDirection(..)
  , CTraitType(..)
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
      :> Capture "categoryId" (Uid Category)
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
      :> Capture "categoryId" (Uid Category)
      :> "notes"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _setCategoryInfo :: route :-
      Summary "Set category's fields"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "categoryId" (Uid Category)
      :> "info"
      :> ReqBody '[JSON] CCategoryInfoEdit
      :> Put '[JSON] NoContent

  , _deleteCategory :: route :-
      Summary "Delete a category"
      :> ErrorResponse 404 "Category not found"
      :> "category"
      :> Capture "categoryId" (Uid Category)
      :> Delete '[JSON] NoContent
  }
  deriving (Generic)

-- | Working with items
data ItemSite route = ItemSite
  { _getItem :: route :-
      Summary "Get item by id"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> Get '[JSON] CItemFull

  , _createItem :: route :-
      Summary "Create a new item in the given category"
      :> Description "Returns the ID of the created item."
      :> ErrorResponse 400 "'name' not provided"
      :> "item"
      :> Capture "categoryId" (Uid Category)
      :> QueryParam' '[Required, Strict] "name" Text
      :> Post '[JSON] (Uid Item)

  , _setItemInfo :: route :-
      Summary "Set item's info"
      :> Description "Note: all fields are optional. If you don't pass a field, \
                     \it won't be modified. To erase a field, send `null`."
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "info"
      :> ReqBody '[JSON] CItemInfoEdit
      :> Put '[JSON] NoContent

  , _setItemSummary :: route :-
      Summary "Set item's summary"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "summary"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _setItemEcosystem :: route :-
      Summary "Set item's ecosystem"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "ecosystem"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _setItemNotes :: route :-
      Summary "Set item's notes"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "notes"
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _deleteItem :: route :-
      Summary "Delete an item"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> Delete '[JSON] NoContent

  , _moveItem :: route :-
      Summary "Move item"
      :> ErrorResponse 404 "Item not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "move"
      :> ReqBody '[JSON] CMove
      :> Post '[JSON] NoContent
  }
  deriving (Generic)

-- | Working with item traits
data TraitSite route = TraitSite
  { _getTrait :: route :-
      Summary "Get trait by id"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "trait"
      :> Capture "traitId" (Uid Trait)
      :> Get '[JSON] CTrait

  ,  _createTrait :: route :-
      Summary "Create a new trait in the given item"
      :> Description "Returns the ID of the created trait."
      :> ErrorResponse 400 "'content' not provided"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "trait"
      :> ReqBody '[JSON] CCreateTrait
      :> Post '[JSON] (Uid Trait)

  , _setTrait :: route :-
      Summary "Update a trait in the given item"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "trait"
      :> Capture "traitId" (Uid Trait)
      :> ReqBody '[JSON] CTextEdit
      :> ErrorResponse 409 "Merge conflict occurred"
      :> Put '[JSON] NoContent

  , _deleteTrait :: route :-
      Summary "Delete a trait"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "trait"
      :> Capture "traitId" (Uid Trait)
      :> Delete '[JSON] NoContent

  , _moveTrait :: route :-
      Summary "Move trait"
      :> ErrorResponse 404 "Item not found"
      :> ErrorResponse 404 "Trait not found"
      :> "item"
      :> Capture "itemId" (Uid Item)
      :> "trait"
      :> Capture "traitId" (Uid Trait)
      :> "move"
      :> ReqBody '[JSON] CMove
      :> Post '[JSON] NoContent
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
data CTraitType = Pro | Con
    deriving (Show, Generic)

instance ToSchema CTraitType where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance A.ToJSON CTraitType where
  toJSON = A.genericToJSON jsonOptions

instance A.FromJSON CTraitType where
  parseJSON = A.genericParseJSON jsonOptions

-- | Direction (Up/Down) for item or trait and their instances.
data CDirection = DirectionUp | DirectionDown
    deriving (Eq, Show, Generic)

instance ToSchema CDirection where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
    { constructorTagModifier = \case
        "DirectionUp" -> "up"
        "DirectionDown" -> "down"
        other -> error ("Direction schema: unknown tag " <> show other)
    }

instance A.ToJSON CDirection where
  toJSON = \case
    DirectionUp -> "up"
    DirectionDown -> "down"

instance A.FromJSON CDirection where
  parseJSON = \case
    "up"   -> pure DirectionUp
    "down" -> pure DirectionDown
    tag    -> fail ("unknown direction " ++ show tag)

----------------------------------------------------------------------------
-- Client types
--
-- These are more "light-weight" Haskell types of 'Guide'.
--
-- Furthermore using these "light-weight" types we keep all data small
-- to send these over the wire w/o having deep nested data,
-- we might not need on front-end.
----------------------------------------------------------------------------

-- | Client type to create new trait.
data CCreateTrait = CCreateTrait
  { cctType    :: CTraitType
  , cctContent :: Text
  } deriving (Show, Generic)

instance A.ToJSON CCreateTrait where
  toJSON = A.genericToJSON jsonOptions

instance A.FromJSON CCreateTrait where
  parseJSON = A.genericParseJSON jsonOptions

instance ToSchema CCreateTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Client type to move trait or item up or down.
data CMove = CMove
  { cmDirection :: CDirection
  } deriving (Show, Eq, Generic)

instance A.ToJSON CMove where
  toJSON = A.genericToJSON jsonOptions

instance A.FromJSON CMove where
  parseJSON = A.genericParseJSON jsonOptions

instance ToSchema CMove where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | A "light-weight" client type of 'Category', which describes a category
-- but doesn't give the notes or the items.
data CCategoryInfo = CCategoryInfo
  { cciId      :: Uid Category
  , cciTitle   :: Text           ? "Category title"
  , cciCreated :: UTCTime        ? "When the category was created"
  , cciGroup_  :: Text           ? "Category group ('grandcategory')"
  , cciStatus  :: CategoryStatus
  }
  deriving (Show, Generic)

instance A.ToJSON CCategoryInfo where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CCategoryInfo where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CCategoryInfo' from a 'Category'
toCCategoryInfo :: Category -> CCategoryInfo
toCCategoryInfo Category{..} = CCategoryInfo
  { cciId      = _categoryUid
  , cciTitle   = H _categoryTitle
  , cciCreated = H _categoryCreated
  , cciGroup_  = H _categoryGroup_
  , cciStatus  = _categoryStatus
  }

-- | A "light-weight" client type of 'Category', which gives all available
-- information about a category
data CCategoryFull = CCategoryFull
  { ccfId          :: Uid Category
  , ccfTitle       :: Text           ? "Category title"
  , ccfGroup       :: Text           ? "Category group ('grandcategory')"
  , ccfStatus      :: CategoryStatus
  , ccfDescription :: CMarkdown
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
  { ccfId          = _categoryUid
  , ccfTitle       = H _categoryTitle
  , ccfGroup       = H _categoryGroup_
  , ccfDescription = toCMarkdown _categoryNotes
  , ccfItems       = H $ fmap toCItemFull _categoryItems
  , ccfStatus      = _categoryStatus
  }

-- | Client type to edit meta category information.
data CCategoryInfoEdit = CCategoryInfoEdit
    { ccieTitle    :: Text            ? "Category title"
    , ccieGroup    :: Text            ? "Category group ('grandcategory')"
    , ccieStatus   :: CategoryStatus
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

-- | A lightweight info type about an 'Item'. Doesn't contain e.g. item
-- traits.
--
-- When updating it, don't forget to update 'CItemInfoEdit' and 'setItemInfo'.
data CItemInfo = CItemInfo
  { ciiId      :: Uid Item
  , ciiCreated :: UTCTime    ? "When the item was created"
  , ciiName    :: Text       ? "Item name"
  , ciiGroup   :: Maybe Text ? "Item group"
  , ciiHackage :: Maybe Text ? "Package name on Hackage"
  , ciiLink    :: Maybe Url  ? "Link to the official site, if exists"
  } deriving (Show, Generic)

instance A.ToJSON CItemInfo where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CItemInfo where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | A type for item edit requests. @Nothing@ means that the field should be
-- left untouched; @Just Nothing@ means that the field should be erased.
data CItemInfoEdit = CItemInfoEdit
  { ciieName    :: Maybe Text         ? "Item name"
  , ciieGroup   :: Maybe (Maybe Text) ? "Item group"
  , ciieHackage :: Maybe (Maybe Text) ? "Package name on Hackage"
  , ciieLink    :: Maybe (Maybe Url)  ? "Link to the official site, if exists"
  } deriving (Show, Generic)

instance A.ToJSON CItemInfoEdit where
  toJSON ciie = A.object $ catMaybes
    [ ("name"    A..=) <$> unH (ciieName ciie)
    , ("group"   A..=) <$> unH (ciieGroup ciie)
    , ("hackage" A..=) <$> unH (ciieHackage ciie)
    , ("link"    A..=) <$> unH (ciieLink ciie)
    ]

instance A.FromJSON CItemInfoEdit where
  parseJSON = A.withObject "CItemInfoEdit" $ \o -> do
    ciieName'    <- o A..:! "name"
    ciieGroup'   <- o A..:! "group"
    ciieHackage' <- o A..:! "hackage"
    ciieLink'    <- o A..:! "link"
    return CItemInfoEdit
      { ciieName    = H ciieName'
      , ciieGroup   = H ciieGroup'
      , ciieHackage = H ciieHackage'
      , ciieLink    = H ciieLink'
      }

instance ToSchema CItemInfoEdit where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Client type of 'Item'
data CItemFull = CItemFull
  { cifId          :: Uid Item
  , cifName        :: Text                     ? "Item name"
  , cifCreated     :: UTCTime                  ? "When the item was created"
  , cifGroup       :: Maybe Text               ? "Item group"
  , cifHackage     :: Maybe Text               ? "Package name on Hackage"
  , cifSummary     :: CMarkdown
  , cifPros        :: [CTrait]                 ? "Pros (positive traits)"
  , cifCons        :: [CTrait]                 ? "Cons (negative traits)"
  , cifEcosystem   :: CMarkdown
  , cifNotes       :: CMarkdown
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
  { ciiId          = _itemUid
  , ciiCreated     = H _itemCreated
  , ciiName        = H _itemName
  , ciiGroup       = H _itemGroup_
  , ciiHackage     = H _itemHackage
  , ciiLink        = H _itemLink
  }

-- | Factory to create a 'CItemFull' from an 'Item'
toCItemFull :: Item -> CItemFull
toCItemFull Item{..} = CItemFull
  { cifId          = _itemUid
  , cifName        = H _itemName
  , cifCreated     = H _itemCreated
  , cifGroup       = H _itemGroup_
  , cifHackage     = H _itemHackage
  , cifSummary     = toCMarkdown _itemSummary
  , cifPros        = H $ fmap toCTrait _itemPros
  , cifCons        = H $ fmap toCTrait _itemCons
  , cifEcosystem   = toCMarkdown _itemEcosystem
  , cifNotes       = toCMarkdown _itemNotes
  , cifLink        = H _itemLink
  , cifToc         = H $ map treeToCMD (markdownTreeMdTOC _itemNotes)
  }
  where
    treeToCMD = fmap toCHeading

-- | Client type of 'Trait'
data CTrait = CTrait
  { ctId      :: Uid Trait
  , ctContent :: CMarkdown
  } deriving (Show, Generic)

instance A.ToJSON CTrait where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CTrait' from a 'Trait'
toCTrait :: Trait -> CTrait
toCTrait trait = CTrait
  { ctId     = trait ^. uid
  , ctContent = toCMarkdown $ trait ^. content
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
  { chContent :: CMarkdown
  , chSlug    :: Text         ? "In-page anchor for linking"
  } deriving (Show, Generic)

instance A.ToJSON CHeading where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CHeading where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

toCHeading :: Heading -> CHeading
toCHeading h = CHeading
  { chContent = toCMarkdown $ headingMd h
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
  { csrcInfo        :: CCategoryInfo
  , csrcDescription :: CMarkdown
  } deriving (Show, Generic)

instance A.ToJSON CSRCategory where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CSRCategory where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | An item was found.
data CSRItem = CSRItem
  { csriCategory    :: CCategoryInfo
  , csriInfo        :: CItemInfo
  , csriSummary     :: Maybe CMarkdown
  , csriEcosystem   :: Maybe CMarkdown
  } deriving (Show, Generic)

instance A.ToJSON CSRItem where
  toJSON = A.genericToJSON jsonOptions

instance ToSchema CSRItem where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
    & mapped.mapped.schema.S.description ?~
      "Note: fields `summary` and `ecosystem` will be present only if the match \
      \was found in those fields."

toCSearchResult :: SearchResult -> CSearchResult
toCSearchResult (SRCategory cat) =
  CSRCategoryResult $ CSRCategory
    { csrcInfo        = toCCategoryInfo cat
    , csrcDescription = toCMarkdown $
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
    { csriCategory    = toCCategoryInfo cat
    , csriInfo        = toCItemInfo item
    , csriSummary     = Just (toCMarkdown (item ^. G.summary))
    , csriEcosystem   = Nothing
    }
-- TODO: currently if there are matches in both item description and item
-- ecosystem, we'll show two matches instead of one
toCSearchResult (SRItemEcosystem cat item) =
  CSRItemResult $ CSRItem
    { csriCategory    = toCCategoryInfo cat
    , csriInfo        = toCItemInfo item
    , csriSummary     = Nothing
    , csriEcosystem   = Just (toCMarkdown (item ^. ecosystem))
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
