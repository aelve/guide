{-# LANGUAGE FlexibleInstances     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | This module provides the Servant API tree, as well as request and
-- response types used by endpoints.
--
-- Many types in this module mirror types from "Guide.Types.Core", but they
-- are more lightweight – i.e. they don't include some data that
-- "Guide.Types.Core" data include, especially nested data. By using
-- lightweight types we keep payloads small. The frontend can always request
-- more data if it needs to.

module Guide.Api.Types
  (
  -- * Routes
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
  , CCreateItem(..)
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

import Data.Tree (Tree(..))
import Lucid (renderText, toHtml)

import Servant
import Servant.API.Generic

import Guide.Api.Error
import Guide.Api.Utils
import Guide.Markdown
import Guide.Search
import Guide.Types.Core as G
import Guide.Uid
import Guide.Utils (Url, fields)

import qualified Data.Aeson as Aeson
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
      :> Description "Returns the ID of the created category."
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
      :> ErrorResponse 400 "'name' can not be empty"
      :> "item"
      :> Capture "categoryId" (Uid Category)
      :> ReqBody '[JSON] CCreateItem
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
      :> ErrorResponse 400 "'content' can not be empty"
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
-- CTraitType
--------------------------------------------------------------------------

-- | Trait type (Pro/Con) and instances.
data CTraitType = CPro | CCon
    deriving (Show, Generic)

instance ToSchema CTraitType where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions
      { constructorTagModifier = \case
          "CPro" -> "Pro"
          "CCon" -> "Con"
          other -> error ("CTraitType schema: unknown value " <> show other)
      }

instance Aeson.ToJSON CTraitType where
  toJSON = \case
    CPro -> "Pro"
    CCon -> "Con"

instance Aeson.FromJSON CTraitType where
  parseJSON = Aeson.withText "CTraitType" $ \case
    "Pro" -> pure CPro
    "Con" -> pure CCon
    other -> fail ("unknown trait type " <> show other)

----------------------------------------------------------------------------
-- CDirection
----------------------------------------------------------------------------

-- | Direction (Up/Down) for item or trait and their instances.
data CDirection = DirectionUp | DirectionDown
    deriving (Eq, Show, Generic)

instance ToSchema CDirection where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
    { constructorTagModifier = \case
        "DirectionUp" -> "up"
        "DirectionDown" -> "down"
        other -> error ("CDirection schema: unknown value " <> show other)
    }

instance Aeson.ToJSON CDirection where
  toJSON = \case
    DirectionUp -> "up"
    DirectionDown -> "down"

instance Aeson.FromJSON CDirection where
  parseJSON = Aeson.withText "CDirection" $ \case
    "up" -> pure DirectionUp
    "down" -> pure DirectionDown
    other -> fail ("unknown direction " <> show other)

----------------------------------------------------------------------------
-- CCreateItem
----------------------------------------------------------------------------

-- | Client type to create new item.
data CCreateItem = CCreateItem
  { cciName    :: Text
  , cciHackage :: Maybe Text
  , cciLink    :: Maybe Url
  } deriving (Show, Generic)

instance Aeson.ToJSON CCreateItem where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CCreateItem where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CCreateItem where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "name" . inlineSchema . description ?= "Item name"
        field "hackage" . inlineSchema . description ?= "Package name on Hackage"
        field "link" . inlineSchema . description ?=
          "Link to the official site, if exists"

----------------------------------------------------------------------------
-- CCreateTrait
----------------------------------------------------------------------------

-- | Client type to create new trait.
data CCreateTrait = CCreateTrait
  { cctType    :: CTraitType
  , cctContent :: Text
  } deriving (Show, Generic)

instance Aeson.ToJSON CCreateTrait where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CCreateTrait where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CCreateTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

----------------------------------------------------------------------------
-- CMove
----------------------------------------------------------------------------

-- | Client type to move trait or item up or down.
data CMove = CMove
  { cmDirection :: CDirection
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON CMove where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CMove where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CMove where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

----------------------------------------------------------------------------
-- CCategoryInfo
----------------------------------------------------------------------------

-- | A "light-weight" client type of 'Category', which describes a category
-- but doesn't give the notes or the items.
data CCategoryInfo = CCategoryInfo
  { cciId      :: Uid Category
  , cciTitle   :: Text
  , cciCreated :: UTCTime
  , cciGroup   :: Text
  , cciStatus  :: CategoryStatus
  }
  deriving (Show, Generic)

instance Aeson.ToJSON CCategoryInfo where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CCategoryInfo where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CCategoryInfo where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "title" . inlineSchema . description ?= "Category title"
        field "created" .= Inline (toSchema (Proxy @UTCTime))
        field "created" . inlineSchema . description ?= "When the category was created"
        field "group" . inlineSchema . description ?= "Category group ('grandcategory')"

-- | Factory to create a 'CCategoryInfo' from a 'Category'
toCCategoryInfo :: Category -> CCategoryInfo
toCCategoryInfo $(fields 'Category) = CCategoryInfo
  { cciId      = categoryUid
  , cciTitle   = categoryTitle
  , cciCreated = categoryCreated
  , cciGroup   = categoryGroup
  , cciStatus  = categoryStatus
  }
  where
    -- Ignored "heavy" fields
    _ = categoryItems
    _ = categoryItemsDeleted
    _ = categoryNotes
    -- Not heavy, just not particularly useful metadata
    _ = categoryEnabledSections

----------------------------------------------------------------------------
-- CCategoryFull
----------------------------------------------------------------------------

-- | A client type of 'Category', which gives all available information
-- about a category including the items contained in it.
data CCategoryFull = CCategoryFull
  { ccfId          :: Uid Category
  , ccfTitle       :: Text
  , ccfGroup       :: Text
  , ccfStatus      :: CategoryStatus
  , ccfDescription :: CMarkdown
  , ccfSections    :: Set ItemSection
  , ccfItems       :: [CItemFull]
  }
  deriving (Show, Generic)

instance Aeson.ToJSON CCategoryFull where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CCategoryFull where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CCategoryFull where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "title" . inlineSchema . description ?= "Category title"
        field "group" . inlineSchema . description ?= "Category group ('grandcategory')"
        field "sections" . inlineSchema . description ?= "Enabled item sections"
        field "items" . inlineSchema . description ?= "All items in the category"

-- | Factory to create a 'CCategoryFull' from a 'Category'
toCCategoryFull :: Category -> CCategoryFull
toCCategoryFull $(fields 'Category) = CCategoryFull
  { ccfId          = categoryUid
  , ccfTitle       = categoryTitle
  , ccfGroup       = categoryGroup
  , ccfStatus      = categoryStatus
  , ccfDescription = toCMarkdown categoryNotes
  , ccfSections    = categoryEnabledSections
  , ccfItems       = fmap toCItemFull categoryItems
  }
  where
    -- Ignored fields
    _ = categoryItemsDeleted
    -- TODO: return creation time
    _ = categoryCreated

----------------------------------------------------------------------------
-- CCategoryInfoEdit
----------------------------------------------------------------------------

-- | Client type to edit meta category information.
data CCategoryInfoEdit = CCategoryInfoEdit
    { ccieTitle    :: Text
    , ccieGroup    :: Text
    , ccieStatus   :: CategoryStatus
    , ccieSections :: Set ItemSection
    }
    deriving (Show, Generic)

instance Aeson.ToJSON CCategoryInfoEdit where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CCategoryInfoEdit where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CCategoryInfoEdit where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "title" . inlineSchema . description ?= "Category title"
        field "group" . inlineSchema . description ?= "Category group ('grandcategory')"
        field "sections" . inlineSchema . description ?= "Enabled item sections"

instance ToSchema ItemSection where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

----------------------------------------------------------------------------
-- CItemInfo
----------------------------------------------------------------------------

-- | A lightweight info type about an 'Item'. Doesn't contain e.g. item
-- traits.
--
-- When updating it, don't forget to update 'CItemInfoEdit' and 'setItemInfo'.
data CItemInfo = CItemInfo
  { ciiId      :: Uid Item
  , ciiCreated :: UTCTime
  , ciiName    :: Text
  , ciiHackage :: Maybe Text
  , ciiLink    :: Maybe Url
  } deriving (Show, Generic)

instance Aeson.ToJSON CItemInfo where
  toJSON = Aeson.genericToJSON jsonOptions

instance ToSchema CItemInfo where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "created" .= Inline (toSchema (Proxy @UTCTime))
        field "created" . inlineSchema . description ?= "When the item was created"
        field "name" . inlineSchema . description ?= "Item name"
        field "hackage" . inlineSchema . description ?= "Package name on Hackage"
        field "link" . inlineSchema . description ?=
          "Link to the official site, if exists"

-- | Factory to create a 'CItemInfo' from an 'Item'
toCItemInfo :: Item -> CItemInfo
toCItemInfo $(fields 'Item) = CItemInfo
  { ciiId          = itemUid
  , ciiCreated     = itemCreated
  , ciiName        = itemName
  , ciiHackage     = itemHackage
  , ciiLink        = itemLink
  }
  where
    -- We don't return "heavy" fields
    _ = itemNotes
    _ = itemEcosystem
    _ = itemSummary
    _ = (itemCons, itemConsDeleted)
    _ = (itemPros, itemProsDeleted)

----------------------------------------------------------------------------
-- CItemInfoEdit
----------------------------------------------------------------------------

-- | A type for item edit requests. @Nothing@ means that the field should be
-- left untouched; @Just Nothing@ means that the field should be erased.
data CItemInfoEdit = CItemInfoEdit
  { ciieName    :: Maybe Text
  , ciieHackage :: Maybe (Maybe Text)
  , ciieLink    :: Maybe (Maybe Url)
  } deriving (Show, Generic)

-- Manual instances because we want special behavior for Maybe
instance Aeson.ToJSON CItemInfoEdit where
  toJSON ciie = Aeson.object $ catMaybes
    [ ("name"    Aeson..=) <$> ciieName ciie
    , ("hackage" Aeson..=) <$> ciieHackage ciie
    , ("link"    Aeson..=) <$> ciieLink ciie
    ]

instance Aeson.FromJSON CItemInfoEdit where
  parseJSON = Aeson.withObject "CItemInfoEdit" $ \o -> do
    ciieName'    <- o Aeson..:! "name"
    ciieHackage' <- o Aeson..:! "hackage"
    ciieLink'    <- o Aeson..:! "link"
    return CItemInfoEdit
      { ciieName    = ciieName'
      , ciieHackage = ciieHackage'
      , ciieLink    = ciieLink'
      }

instance ToSchema CItemInfoEdit where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "name" . inlineSchema . description ?= "Item name"
        field "hackage" . inlineSchema . description ?= "Package name on Hackage"
        field "link" . inlineSchema . description ?=
          "Link to the official site, if exists"

----------------------------------------------------------------------------
-- CItemFull
----------------------------------------------------------------------------

-- | Client type of 'Item'
data CItemFull = CItemFull
  { cifId          :: Uid Item
  , cifName        :: Text
  , cifCreated     :: UTCTime
  , cifHackage     :: Maybe Text
  , cifSummary     :: CMarkdown
  , cifPros        :: [CTrait]
  , cifCons        :: [CTrait]
  , cifEcosystem   :: CMarkdown
  , cifNotes       :: CMarkdown
  , cifLink        :: Maybe Url
  , cifToc         :: [CTocHeading]
  } deriving (Show, Generic)

instance Aeson.ToJSON CItemFull where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CItemFull where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CItemFull where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "name" . inlineSchema . description ?= "Item name"
        field "created" .= Inline (toSchema (Proxy @UTCTime))
        field "created" . inlineSchema . description ?= "When the item was created"
        field "hackage" . inlineSchema . description ?= "Package name on Hackage"
        field "pros" . inlineSchema . description ?= "Pros (positive traits)"
        field "cons" . inlineSchema . description ?= "Cons (negative traits)"
        field "link" . inlineSchema . description ?=
          "Link to the official site, if exists"
        field "toc" . inlineSchema . description ?= "Table of contents"

-- | Factory to create a 'CItemFull' from an 'Item'
toCItemFull :: Item -> CItemFull
toCItemFull $(fields 'Item) = CItemFull
  { cifId          = itemUid
  , cifName        = itemName
  , cifCreated     = itemCreated
  , cifHackage     = itemHackage
  , cifSummary     = toCMarkdown itemSummary
  , cifPros        = fmap toCTrait itemPros
  , cifCons        = fmap toCTrait itemCons
  , cifEcosystem   = toCMarkdown itemEcosystem
  , cifNotes       = toCMarkdown itemNotes
  , cifLink        = itemLink
  , cifToc         = map toCTocHeading (markdownTreeTOC itemNotes)
  }
  where
    -- Ignored fields
    _ = (itemProsDeleted, itemConsDeleted)

----------------------------------------------------------------------------
-- CTrait
----------------------------------------------------------------------------

-- | Client type of 'Trait'
data CTrait = CTrait
  { ctId      :: Uid Trait
  , ctContent :: CMarkdown
  } deriving (Show, Generic)

instance Aeson.ToJSON CTrait where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CTrait where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CTrait where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

-- | Factory to create a 'CTrait' from a 'Trait'
toCTrait :: Trait -> CTrait
toCTrait $(fields 'Trait) = CTrait
  { ctId      = traitUid
  , ctContent = toCMarkdown traitContent
  }

----------------------------------------------------------------------------
-- CMarkdown
----------------------------------------------------------------------------

-- | Client type of 'Markdown'
data CMarkdown = CMarkdown
  { cmdText :: Text
  , cmdHtml :: Text
  } deriving (Show, Generic)

instance Aeson.ToJSON CMarkdown where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CMarkdown where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CMarkdown where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "text" . inlineSchema . description ?= "Markdown source"
        field "html" . inlineSchema . description ?= "Rendered HTML"

-- | Type class to create 'CMarkdown'
class ToCMarkdown md where toCMarkdown :: md -> CMarkdown

instance ToCMarkdown MarkdownInline where
  toCMarkdown $(fields 'MarkdownInline) = CMarkdown
    { cmdText = markdownInlineSource
    , cmdHtml = utf8ToText markdownInlineHtml
    }
    where
      -- Ignored fields
      _ = markdownInlineMarkdown

instance ToCMarkdown MarkdownBlock where
  toCMarkdown $(fields 'MarkdownBlock) = CMarkdown
    { cmdText = markdownBlockSource
    , cmdHtml = utf8ToText markdownBlockHtml
    }
    where
      -- Ignored fields
      _ = markdownBlockMarkdown

instance ToCMarkdown MarkdownTree where
  toCMarkdown md@($(fields 'MarkdownTree)) = CMarkdown
    { cmdText = markdownTreeSource
    , cmdHtml = toText . renderText $ toHtml md
    }
    where
      -- Ignored fields
      _ = markdownTreeStructure
      _ = markdownTreeTOC
      _ = markdownTreeIdPrefix

----------------------------------------------------------------------------
-- CTocHeading
----------------------------------------------------------------------------

-- | Frontend's table of content type used in items' stuff.
data CTocHeading = CTocHeading
  { cthContent     :: CMarkdown
  , cthSlug        :: Text
  , cthSubheadings :: [CTocHeading]
  } deriving (Show, Generic)

instance Aeson.ToJSON CTocHeading where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CTocHeading where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CTocHeading where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "slug" . inlineSchema . description ?= "In-page anchor for linking"

-- | 'toCTocHeading' converts a table of contents into the format expected by the frontend.
toCTocHeading :: Tree Heading -> CTocHeading
toCTocHeading $(fields 'Node) = CTocHeading
  { cthContent     = toCMarkdown $ headingMarkdown rootLabel
  , cthSlug        = headingSlug rootLabel
  , cthSubheadings = map toCTocHeading subForest
  }

----------------------------------------------------------------------------
-- CTextEdit
----------------------------------------------------------------------------

-- | Frontend sends this type to edit notes or descriptions.
data CTextEdit = CTextEdit
  { cteOriginal :: Text
  , cteModified :: Text
  } deriving (Show, Generic)

instance Aeson.ToJSON CTextEdit where
  toJSON = Aeson.genericToJSON jsonOptions

instance Aeson.FromJSON CTextEdit where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance ToSchema CTextEdit where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "original" . inlineSchema . description ?= "State of base before editing"
        field "modified" . inlineSchema . description ?= "Modified text"

----------------------------------------------------------------------------
-- CMergeConflict
----------------------------------------------------------------------------

-- | Backend returns this type if there is conflict between state of base
-- before and after editing.
data CMergeConflict = CMergeConflict
  { cmcOriginal       :: Text
  , cmcModified       :: Text
  , cmcServerModified :: Text
  , cmcMerged         :: Text
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON CMergeConflict where
  toJSON = Aeson.genericToJSON jsonOptions

instance ToSchema CMergeConflict where
  declareNamedSchema p = do
    schema_ <- genericDeclareNamedSchema schemaOptions p
    pure $ schema_ &~ do
      zoom (S.schema . properties) $ do
        field "original" . inlineSchema . description ?= "State of base before editing"
        field "modified" . inlineSchema . description ?= "Modified text"
        field "server_modified" . inlineSchema . description ?=
          "State of base after editing by someone else (i.e. text got changed \
          \on the server)"
        field "merged" . inlineSchema . description ?= "An attempt to merge edits"

----------------------------------------------------------------------------
-- CSearchResult
----------------------------------------------------------------------------

-- | Client type of 'SearchResult'
data CSearchResult
  -- | Match was found in category title
  = CSRCategoryResult CSRCategory
  -- | Match was found in the item
  | CSRItemResult CSRItem
  deriving (Show, Generic)

instance Aeson.ToJSON CSearchResult where
  toJSON = \case
    CSRCategoryResult cat -> Aeson.object
      [ "tag" Aeson..= ("Category" :: Text)
      , "contents" Aeson..= cat
      ]
    CSRItemResult item -> Aeson.object
      [ "tag" Aeson..= ("Item" :: Text)
      , "contents" Aeson..= item
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

----------------------------------------------------------------------------
-- CSRCategory
----------------------------------------------------------------------------

-- | A category was found.
data CSRCategory = CSRCategory
  { csrcInfo        :: CCategoryInfo
  , csrcDescription :: CMarkdown
  } deriving (Show, Generic)

instance Aeson.ToJSON CSRCategory where
  toJSON = Aeson.genericToJSON jsonOptions

instance ToSchema CSRCategory where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

----------------------------------------------------------------------------
-- CSRItem
----------------------------------------------------------------------------

-- | An item was found.
data CSRItem = CSRItem
  { csriCategory    :: CCategoryInfo
  , csriInfo        :: CItemInfo
  , csriSummary     :: Maybe CMarkdown
  , csriEcosystem   :: Maybe CMarkdown
  } deriving (Show, Generic)

instance Aeson.ToJSON CSRItem where
  toJSON = Aeson.genericToJSON jsonOptions

instance ToSchema CSRItem where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
    & mapped.mapped.schema.S.description ?~
      "Note: fields `summary` and `ecosystem` will be present only if the match \
      \was found in those fields."

----------------------------------------------------------------------------
-- toCSearchResult
----------------------------------------------------------------------------

-- | Create a 'CSearchResult' from a 'SearchResult'.
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
        extractPreface $ toMarkdownTree "" $ markdownBlockSource (categoryNotes cat)
    }
toCSearchResult (SRItem cat item) =
  CSRItemResult $ CSRItem
    { csriCategory    = toCCategoryInfo cat
    , csriInfo        = toCItemInfo item
    , csriSummary     = Just (toCMarkdown (itemSummary item))
    , csriEcosystem   = Nothing
    }
-- TODO: currently if there are matches in both item description and item
-- ecosystem, we'll show two matches instead of one
toCSearchResult (SRItemEcosystem cat item) =
  CSRItemResult $ CSRItem
    { csriCategory    = toCCategoryInfo cat
    , csriInfo        = toCItemInfo item
    , csriSummary     = Nothing
    , csriEcosystem   = Just (toCMarkdown (itemEcosystem item))
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

instance ToSchema a => ToSchema (Tree a) where
    declareNamedSchema = genericDeclareNamedSchema schemaOptions
