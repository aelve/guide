{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

-- | Types for postgres database
module Guide.Database.Types
       (
       -- * Types
         DatabaseError(..)
       , CategoryRow (..)
       , ItemRow (..)
       , TraitRow (..)

       -- * Type convertions
       , categoryRowToCategory
       , categoryToRowCategory
       , itemRowToItem
       , itemToRowItem
       , traitRowToTrait
       , traitToTraitRow

       ) where

import Imports

import Named

import Guide.Markdown (toMarkdownBlock, toMarkdownTree, toMarkdownInline)
import Guide.Types.Core (Category (..), CategoryStatus, Item (..), ItemSection, Trait (..),
                         TraitType)
import Guide.Utils (Uid (..))


-- | Custom datatype errors for database
data DatabaseError
  = ItemNotFound (Uid Item)
  | CategoryNotFound (Uid Category)
  | TraitNotFound (Uid Trait)
  | ItemAlreadyInCategory (Uid Category) (Uid Item)
  | TraitAlreadyInItem (Uid Item) (Uid Trait)
  | ItemNotInCategory (Uid Category) (Uid Item)
  | TraitNotInItem (Uid Item) (Uid Trait)
  deriving Show

-- | Category intermediary type.
data CategoryRow = CategoryRow
  { categoryRowUid        :: Uid Category
  , categoryRowTitle      :: Text
  , categoryRowCreated    :: UTCTime
  , categoryRowGroup      :: Text
  , categoryRowStatus     :: CategoryStatus
  , categoryRowNotes      :: Text
  , categoryRowSelections :: Set ItemSection
  , categoryRowItemOrder  :: [Uid Item]
  } deriving Show

-- | Item intermediary type.
data ItemRow = ItemRow
  { itemRowUid         :: Uid Item
  , itemRowName        :: Text
  , itemRowCreated     :: UTCTime
  , itemRowLink        :: Maybe Text
  , itemRowHackage     :: Maybe Text
  , itemRowSummary     :: Text
  , itemRowEcosystem   :: Text
  , itemRowNotes       :: Text
  , itemRowDeleted     :: Bool
  , itemRowCategoryUid :: Uid Category
  , itemRowProsOrder   :: [Uid Trait]
  , itemRowConsOrder   :: [Uid Trait]
  } deriving Show

-- | Trait intermediary type.
data TraitRow = TraitRow
  { traitRowUid     :: Uid Trait
  , traitRowContent :: Text
  , traitRowDeleted :: Bool
  , traitRowType    :: TraitType
  , traitRowItemUid :: Uid Item
  } deriving Show

----------------------------------------------------------------------------
-- Convertions between types
----------------------------------------------------------------------------

-- | Convert CategoryRow to Category.
--
-- | To fetch items (they have an order) use 'getItemRowsByCategory' from 'Get' module.
-- | To fetch deleted items use 'getDeletedItemRowsByCategory' from 'Get' module
categoryRowToCategory
  :: "items" :! [Item]
  -> "itemsDeleted" :! [Item]
  -> CategoryRow
  -> Category
categoryRowToCategory
  (arg #items -> items)
  (arg #itemsDeleted -> itemsDeleted)
  CategoryRow{..}
  =
  Category
    { _categoryUid = categoryRowUid
    , _categoryTitle = categoryRowTitle
    , _categoryCreated = categoryRowCreated
    , _categoryGroup_ = categoryRowGroup
    , _categoryStatus = categoryRowStatus
    , _categoryNotes = toMarkdownBlock categoryRowNotes
    , _categoryItems = items
    , _categoryItemsDeleted = itemsDeleted
    , _categoryEnabledSections = categoryRowSelections
    }

-- | Convert Category to CategoryRow.
categoryToRowCategory :: Category -> CategoryRow
categoryToRowCategory Category {..} = CategoryRow
  { categoryRowUid = _categoryUid
  , categoryRowTitle = _categoryTitle
  , categoryRowCreated = _categoryCreated
  , categoryRowGroup = _categoryGroup_
  , categoryRowStatus = _categoryStatus
  , categoryRowNotes = toText $ show _categoryNotes
  , categoryRowSelections = _categoryEnabledSections
  , categoryRowItemOrder = map _itemUid _categoryItems
  }

-- | Convert ItemRow to Item.
--
-- | To fetch traits (they have an order) use 'getTraitRowsByItem' from 'Get' module.
-- | To fetch deleted traits use 'getDeletedTraitRowsByItem' from 'Get' module
itemRowToItem
  :: "proTraits" :! [Trait]
  -> "proDeletedTraits" :! [Trait]
  -> "conTraits" :! [Trait]
  -> "conDeletedTraits" :! [Trait]
  -> ItemRow
  -> Item
itemRowToItem
  (arg #proTraits -> proTraits)
  (arg #proDeletedTraits -> proDeletedTraits)
  (arg #conTraits -> conTraits)
  (arg #conDeletedTraits -> conDeletedTraits)
  ItemRow{..}
  =
  Item
    { _itemUid = itemRowUid
    , _itemName = itemRowName
    , _itemCreated = itemRowCreated
    , _itemHackage = itemRowHackage
    , _itemSummary = toMarkdownBlock itemRowSummary
    , _itemPros = proTraits
    , _itemProsDeleted = proDeletedTraits
    , _itemCons = conTraits
    , _itemConsDeleted = conDeletedTraits
    , _itemEcosystem = toMarkdownBlock itemRowEcosystem
    , _itemNotes = toMarkdownTree prefix itemRowNotes
    , _itemLink = itemRowLink
    }
  where
    prefix = "item-notes-" <> uidToText itemRowUid <> "-"

-- | Convert Item to ItemRow.
itemToRowItem :: Uid Category -> "deleted" :! Bool -> Item -> ItemRow
itemToRowItem catId (arg #deleted -> deleted) Item{..} = ItemRow
  { itemRowUid = _itemUid
  , itemRowName = _itemName
  , itemRowCreated = _itemCreated
  , itemRowLink = _itemLink
  , itemRowHackage = _itemHackage
  , itemRowSummary = toText $ show _itemSummary
  , itemRowEcosystem = toText $ show _itemEcosystem
  , itemRowNotes = toText $ show _itemNotes
  , itemRowDeleted = deleted
  , itemRowCategoryUid = catId
  , itemRowProsOrder = map _traitUid _itemPros
  , itemRowConsOrder = map _traitUid _itemCons
  }

-- | Convert TraitRow to Trait.
traitRowToTrait :: TraitRow -> Trait
traitRowToTrait TraitRow{..} = Trait
  { _traitUid = traitRowUid
  , _traitContent = toMarkdownInline traitRowContent
  }

-- Convert Trait to TraitRow
traitToTraitRow
  :: Uid Item
  -> "deleted" :! Bool
  -> TraitType
  -> Trait
  -> TraitRow
traitToTraitRow itemId (arg #deleted -> deleted) traitType Trait{..} =
  TraitRow
    { traitRowUid = _traitUid
    , traitRowContent = toText $ show _traitContent
    , traitRowDeleted = deleted
    , traitRowType = traitType
    , traitRowItemUid = itemId
    }
