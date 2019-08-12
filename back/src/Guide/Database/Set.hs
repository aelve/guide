{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

-- | Update queries.
module Guide.Database.Set
       (
       -- * Category
         modifyCategoryRow
       , deleteCategory

       -- * Item
       , modifyItemRow
       , deleteItem

       -- * Trait
       , modifyTraitRow
       , deleteTrait

       ) where

import Imports

import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Text.RawString.QQ (r)

import qualified Hasql.Transaction as HT

import Guide.Database.Get
import Guide.Database.Types
import Guide.Database.Utils
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..), CategoryStatus (..), ItemSection (..))
import Guide.Utils (Uid (..), fieldsPrefixed)


----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Fetch a row corresponding to a category, apply a function and write it
-- back. You can break database invariants with this function, so be
-- careful.
--
-- This function takes care to only write the fields that were modified.
--
-- Fields 'categoryRowUid' and 'categoryRowCreated' can not be modified. An
-- attempt to modify them would result in 'CategoryRowUpdateNotAllowed'.
modifyCategoryRow
  :: Uid Category
  -> (CategoryRow -> CategoryRow)
  -> ExceptT DatabaseError Transaction ()
modifyCategoryRow catId f = do
  -- Fetch the old row
  row <- getCategoryRow catId

  -- Expose all fields of the old and the new row, and make sure that if we
  -- forget to use one of them, the compiler will warn us.
  let $(fieldsPrefixed "old_" 'CategoryRow) = row
      $(fieldsPrefixed "new_" 'CategoryRow) = f row

  -- Updating uid is not allowed
  when (old_categoryRowUid /= new_categoryRowUid) $
    throwError CategoryRowUpdateNotAllowed
      { deCategoryId = catId
      , deFieldName = "categoryRowUid" }

  -- Updating creation time is not allowed
  when (old_categoryRowCreated /= new_categoryRowCreated) $
    throwError CategoryRowUpdateNotAllowed
      { deCategoryId = catId
      , deFieldName = "categoryRowCreated" }

  -- Update title
  when (old_categoryRowTitle /= new_categoryRowTitle) $ do
    let statement :: Statement (Uid Category, Text) ()
        statement = execute
          [r|UPDATE categories SET title = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_categoryRowTitle) statement

  -- Update group
  when (old_categoryRowGroup /= new_categoryRowGroup) $ do
    let statement :: Statement (Uid Category, Text) ()
        statement = execute
          [r|UPDATE categories SET group_ = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_categoryRowGroup) statement

  -- Update status
  when (old_categoryRowStatus /= new_categoryRowStatus) $ do
    let statement :: Statement (Uid Category, CategoryStatus) ()
        statement = execute
          [r|UPDATE categories SET status = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_categoryRowStatus) statement

  -- Update notes
  when (old_categoryRowNotes /= new_categoryRowNotes) $ do
    let statement :: Statement (Uid Category, Text) ()
        statement = execute
          [r|UPDATE categories SET notes = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_categoryRowNotes) statement

  -- Update enabled sections
  when (old_categoryRowEnabledSections /= new_categoryRowEnabledSections) $ do
    let statement :: Statement (Uid Category, Set ItemSection) ()
        statement = execute
          [r|UPDATE categories SET enabled_sections = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_categoryRowEnabledSections) statement

  -- Update item order
  when (old_categoryRowItemsOrder /= new_categoryRowItemsOrder) $ do
    let statement :: Statement (Uid Category, [Uid Item]) ()
        statement = execute
          [r|UPDATE categories SET items_order = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_categoryRowItemsOrder) statement

-- | Delete category completly.
deleteCategory :: Uid Category -> ExceptT DatabaseError Transaction ()
deleteCategory catId = do
  let statement :: Statement (Identity (Uid Category)) ()
      statement = execute
        [r|
          DELETE FROM categories
          WHERE uid = $1
        |]
  lift $ HT.statement (Identity catId) statement
  -- Items belonging to the category will be deleted automatically because
  -- of "ON DELETE CASCADE" in the table schema.

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Fetch a row corresponding to an item, apply a function and write it
-- back. You can break database invariants with this function, so be
-- careful.
--
-- This function takes care to only write the fields that were modified.
--
-- Fields 'itemRowUid' and 'itemRowCreated' can not be modified. An attempt
-- to modify them would result in 'ItemRowUpdateNotAllowed'.
modifyItemRow
  :: Uid Item
  -> (ItemRow -> ItemRow)
  -> ExceptT DatabaseError Transaction ()
modifyItemRow itemId f = do
  -- Fetch the old row
  row <- getItemRow itemId

  -- Expose all fields of the old and the new row, and make sure that if we
  -- forget to use one of them, the compiler will warn us.
  let $(fieldsPrefixed "old_" 'ItemRow) = row
      $(fieldsPrefixed "new_" 'ItemRow) = f row

  -- Updating uid is not allowed
  when (old_itemRowUid /= new_itemRowUid) $
    throwError ItemRowUpdateNotAllowed
      { deItemId = itemId
      , deFieldName = "itemRowUid" }

  -- Updating creation time is not allowed
  when (old_itemRowCreated /= new_itemRowCreated) $
    throwError ItemRowUpdateNotAllowed
      { deItemId = itemId
      , deFieldName = "itemRowCreated" }

  -- Update name
  when (old_itemRowName /= new_itemRowName) $ do
    let statement :: Statement (Uid Item, Text) ()
        statement = execute
          [r|UPDATE items SET name = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowName) statement

  -- Update link
  when (old_itemRowLink /= new_itemRowLink) $ do
    let statement :: Statement (Uid Item, Maybe Text) ()
        statement = execute
          [r|UPDATE items SET link = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowLink) statement

  -- Update hackage
  when (old_itemRowHackage /= new_itemRowHackage) $ do
    let statement :: Statement (Uid Item, Maybe Text) ()
        statement = execute
          [r|UPDATE items SET hackage = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowHackage) statement

  -- Update summary
  when (old_itemRowSummary /= new_itemRowSummary) $ do
    let statement :: Statement (Uid Item, Text) ()
        statement = execute
          [r|UPDATE items SET summary = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowSummary) statement

  -- Update ecosystem
  when (old_itemRowEcosystem /= new_itemRowEcosystem) $ do
    let statement :: Statement (Uid Item, Text) ()
        statement = execute
          [r|UPDATE items SET ecosystem = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowEcosystem) statement

  -- Update notes
  when (old_itemRowNotes /= new_itemRowNotes) $ do
    let statement :: Statement (Uid Item, Text) ()
        statement = execute
          [r|UPDATE items SET notes = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowNotes) statement

  -- Update deleted
  when (old_itemRowDeleted /= new_itemRowDeleted) $ do
    let statement :: Statement (Uid Item, Bool) ()
        statement = execute
          [r|UPDATE items SET deleted = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowDeleted) statement

  -- Update categoryUid
  when (old_itemRowCategoryUid /= new_itemRowCategoryUid) $ do
    let statement :: Statement (Uid Item, Uid Category) ()
        statement = execute
          [r|UPDATE items SET category_uid = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowCategoryUid) statement

  -- Update prosOrder
  when (old_itemRowProsOrder /= new_itemRowProsOrder) $ do
    let statement :: Statement (Uid Item, [Uid Trait]) ()
        statement = execute
          [r|UPDATE items SET pros_order = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowProsOrder) statement

  -- Update consOrder
  when (old_itemRowConsOrder /= new_itemRowConsOrder) $ do
    let statement :: Statement (Uid Item, [Uid Trait]) ()
        statement = execute
          [r|UPDATE items SET cons_order = $2 WHERE uid = $1|]
    lift $ HT.statement (itemId, new_itemRowConsOrder) statement

-- | Delete item completly.
deleteItem :: Uid Item -> ExceptT DatabaseError Transaction ()
deleteItem itemId = do
  catId <- getCategoryIdByItem itemId
  let statement :: Statement (Identity (Uid Item)) ()
      statement = execute
        [r|
          DELETE FROM items
          WHERE uid = $1
        |]
  lift $ HT.statement (Identity itemId) statement
  modifyCategoryRow catId $
    _categoryRowItemsOrder %~ delete itemId
  -- Traits belonging to the item will be deleted automatically because of
  -- "ON DELETE CASCADE" in the table schema.

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Fetch a row corresponding to a trait, apply a function and write it
-- back. You can break database invariants with this function, so be
-- careful.
--
-- This function takes care to only write the fields that were modified.
--
-- Field 'traitRowUid' can not be modified. An attempt to modify it would
-- result in 'TraitRowUpdateNotAllowed'.
modifyTraitRow
  :: Uid Trait
  -> (TraitRow -> TraitRow)
  -> ExceptT DatabaseError Transaction ()
modifyTraitRow catId f = do
  -- Fetch the old row
  row <- getTraitRow catId

  -- Expose all fields of the old and the new row, and make sure that if we
  -- forget to use one of them, the compiler will warn us.
  let $(fieldsPrefixed "old_" 'TraitRow) = row
      $(fieldsPrefixed "new_" 'TraitRow) = f row

  -- Updating uid is not allowed
  when (old_traitRowUid /= new_traitRowUid) $
    throwError TraitRowUpdateNotAllowed
      { deTraitId = catId
      , deFieldName = "traitRowUid" }

  -- Update content
  when (old_traitRowContent /= new_traitRowContent) $ do
    let statement :: Statement (Uid Trait, Text) ()
        statement = execute
          [r|UPDATE traits SET content = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_traitRowContent) statement

  -- Update deleted
  when (old_traitRowDeleted /= new_traitRowDeleted) $ do
    let statement :: Statement (Uid Trait, Bool) ()
        statement = execute
          [r|UPDATE traits SET deleted = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_traitRowDeleted) statement

  -- Update type
  when (old_traitRowType /= new_traitRowType) $ do
    let statement :: Statement (Uid Trait, TraitType) ()
        statement = execute
          [r|UPDATE traits SET type_ = ($2 :: trait_type) WHERE uid = $1|]
    lift $ HT.statement (catId, new_traitRowType) statement

  -- Update itemUid
  when (old_traitRowItemUid /= new_traitRowItemUid) $ do
    let statement :: Statement (Uid Trait, Uid Item) ()
        statement = execute
          [r|UPDATE traits SET item_uid = $2 WHERE uid = $1|]
    lift $ HT.statement (catId, new_traitRowItemUid) statement

-- | Delete trait completly.
deleteTrait :: Uid Trait -> ExceptT DatabaseError Transaction ()
deleteTrait traitId = do
  itemId <- getItemIdByTrait traitId
  traitType <- traitRowType <$> getTraitRow traitId
  let statement :: Statement (Identity (Uid Trait)) ()
      statement = execute
        [r|
          DELETE FROM traits
          WHERE uid = $1
        |]
  lift $ HT.statement (Identity traitId) statement
  case traitType of
    TraitTypePro ->
      modifyItemRow itemId $
        _itemRowProsOrder %~ delete traitId
    TraitTypeCon ->
      modifyItemRow itemId $
        _itemRowConsOrder %~ delete traitId
