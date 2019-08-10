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

import Contravariant.Extras.Contrazip (contrazip2)
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Text.RawString.QQ (r)

import qualified Hasql.Decoders as HD
import qualified Hasql.Transaction as HT

import Guide.Database.Convert
import Guide.Database.Get
import Guide.Database.Types
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..))
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
    let sql = [r|UPDATE categories SET title = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_categoryRowTitle)
      (Statement sql encoder decoder False)

  -- Update group
  when (old_categoryRowGroup /= new_categoryRowGroup) $ do
    let sql = [r|UPDATE categories SET group_ = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_categoryRowGroup)
      (Statement sql encoder decoder False)

  -- Update status
  when (old_categoryRowStatus /= new_categoryRowStatus) $ do
    let sql = [r|UPDATE categories SET status = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam categoryStatusParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_categoryRowStatus)
      (Statement sql encoder decoder False)

  -- Update notes
  when (old_categoryRowNotes /= new_categoryRowNotes) $ do
    let sql = [r|UPDATE categories SET notes = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_categoryRowNotes)
      (Statement sql encoder decoder False)

  -- Update enabled sections
  when (old_categoryRowEnabledSections /= new_categoryRowEnabledSections) $ do
    let sql = [r|UPDATE categories SET enabled_sections = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam itemSectionSetParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_categoryRowEnabledSections)
      (Statement sql encoder decoder False)

  -- Update item order
  when (old_categoryRowItemsOrder /= new_categoryRowItemsOrder) $ do
    let sql = [r|UPDATE categories SET items_order = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam uidsParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_categoryRowItemsOrder)
      (Statement sql encoder decoder False)

-- | Delete category completly.
deleteCategory :: Uid Category -> ExceptT DatabaseError Transaction ()
deleteCategory catId = do
  let sql = [r|
        DELETE FROM categories
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.noResult
  lift $ HT.statement catId (Statement sql encoder decoder False)
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
    let sql = [r|UPDATE items SET name = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowName)
      (Statement sql encoder decoder False)

  -- Update link
  when (old_itemRowLink /= new_itemRowLink) $ do
    let sql = [r|UPDATE items SET link = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParamNullable
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowLink) (Statement sql encoder decoder False)

  -- Update hackage
  when (old_itemRowHackage /= new_itemRowHackage) $ do
    let sql = [r|UPDATE items SET hackage = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParamNullable
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowHackage)
      (Statement sql encoder decoder False)

  -- Update summary
  when (old_itemRowSummary /= new_itemRowSummary) $ do
    let sql = [r|UPDATE items SET summary = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowSummary)
      (Statement sql encoder decoder False)

  -- Update ecosystem
  when (old_itemRowEcosystem /= new_itemRowEcosystem) $ do
    let sql = [r|UPDATE items SET ecosystem = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowEcosystem)
      (Statement sql encoder decoder False)

  -- Update notes
  when (old_itemRowNotes /= new_itemRowNotes) $ do
    let sql = [r|UPDATE items SET notes = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowNotes)
      (Statement sql encoder decoder False)

  -- Update deleted
  when (old_itemRowDeleted /= new_itemRowDeleted) $ do
    let sql = [r|UPDATE items SET deleted = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam boolParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowDeleted)
      (Statement sql encoder decoder False)

  -- Update categoryUid
  when (old_itemRowCategoryUid /= new_itemRowCategoryUid) $ do
    let sql = [r|UPDATE items SET category_uid = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam uidParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowCategoryUid)
      (Statement sql encoder decoder False)

  -- Update prosOrder
  when (old_itemRowProsOrder /= new_itemRowProsOrder) $ do
    let sql = [r|UPDATE items SET pros_order = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam uidsParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowProsOrder)
      (Statement sql encoder decoder False)

  -- Update consOrder
  when (old_itemRowConsOrder /= new_itemRowConsOrder) $ do
    let sql = [r|UPDATE items SET cons_order = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam uidsParam
        decoder = HD.noResult
    lift $ HT.statement (itemId, new_itemRowConsOrder)
      (Statement sql encoder decoder False)

-- | Delete item completly.
deleteItem :: Uid Item -> ExceptT DatabaseError Transaction ()
deleteItem itemId = do
  catId <- getCategoryIdByItem itemId
  let sql = [r|
        DELETE FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.noResult
  lift $ HT.statement itemId (Statement sql encoder decoder False)
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
    let sql = [r|UPDATE traits SET content = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam textParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_traitRowContent)
      (Statement sql encoder decoder False)

  -- Update deleted
  when (old_traitRowDeleted /= new_traitRowDeleted) $ do
    let sql = [r|UPDATE traits SET deleted = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam boolParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_traitRowDeleted)
      (Statement sql encoder decoder False)

  -- Update type
  when (old_traitRowType /= new_traitRowType) $ do
    let sql = [r|UPDATE traits SET type_ = ($2 :: trait_type) WHERE uid = $1|]
        encoder = contrazip2 uidParam traitTypeParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_traitRowType)
      (Statement sql encoder decoder False)

  -- Update itemUid
  when (old_traitRowItemUid /= new_traitRowItemUid) $ do
    let sql = [r|UPDATE traits SET item_uid = $2 WHERE uid = $1|]
        encoder = contrazip2 uidParam uidParam
        decoder = HD.noResult
    lift $ HT.statement (catId, new_traitRowItemUid)
      (Statement sql encoder decoder False)

-- | Delete trait completly.
deleteTrait :: Uid Trait -> ExceptT DatabaseError Transaction ()
deleteTrait traitId = do
  itemId <- getItemIdByTrait traitId
  traitType <- traitRowType <$> getTraitRow traitId
  let sql = [r|
        DELETE FROM traits
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.noResult
  lift $ HT.statement traitId (Statement sql encoder decoder False)
  case traitType of
    TraitTypePro ->
      modifyItemRow itemId $
        _itemRowProsOrder %~ delete traitId
    TraitTypeCon ->
      modifyItemRow itemId $
        _itemRowConsOrder %~ delete traitId
