{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}


-- | Read-only database queries.
module Guide.Database.Queries.Select
(
  -- * Category
  selectCategoryRow,
  selectCategoryRowMaybe,
  selectCategoryRows,
  selectCategoryRowByItemMaybe,
  selectCategoryIdByItem,
  selectCategoryIdByItemMaybe,

  -- * Item
  selectItemRow,
  selectItemRowMaybe,
  selectItemRowsByCategory,
  selectDeletedItemRowsByCategory,
  selectItemIdByTrait,
  selectItemIdByTraitMaybe,

  -- * Trait
  selectTraitRowMaybe,
  selectTraitRow,
  selectTraitRowsByItem,
  selectDeletedTraitRowsByItem,
)
where

import Imports

import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Data.Profunctor (lmap, rmap, dimap)

import qualified Hasql.Transaction as HT

import Guide.Database.Types
import Guide.Database.Utils
import Guide.Types.Core
import Guide.Utils (Uid (..))

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get a 'CategoryRow'.
selectCategoryRowMaybe :: Uid Category -> ExceptT DatabaseError Transaction (Maybe CategoryRow)
selectCategoryRowMaybe catId = do
  let statement :: Statement (Uid Category) (Maybe CategoryRow)
      statement = lmap SingleParam $
        [queryRowMaybe|
          SELECT
              uid
            , title
            , created
            , group_
            , status
            , notes
            , enabled_sections
            , items_order
            , deleted
          FROM categories
          WHERE uid = $1
        |]
  lift $ HT.statement catId statement

-- | Get a 'CategoryRow'.
--
-- Fails with 'CategoryNotFound' when the category does not exist.
selectCategoryRow :: Uid Category -> ExceptT DatabaseError Transaction CategoryRow
selectCategoryRow catId = do
  mCatRow <- selectCategoryRowMaybe catId
  case mCatRow of
    Nothing     -> throwError $ CategoryNotFound catId
    Just catRow -> pure catRow

-- | Get the ID of the category that an item belongs to.
selectCategoryIdByItemMaybe
  :: Uid Item -> ExceptT DatabaseError Transaction (Maybe (Uid Category))
selectCategoryIdByItemMaybe itemId = do
  let statement :: Statement (Uid Item) (Maybe (Uid Category))
      statement = dimap SingleParam (fmap fromSingleColumn) $
        [queryRowMaybe|
          SELECT category_uid
          FROM items
          WHERE uid = $1
        |]
  lift $ HT.statement itemId statement

-- | Get an ID of the category that an item belongs to.
--
-- Throw error if item not found.
selectCategoryIdByItem :: Uid Item -> ExceptT DatabaseError Transaction (Uid Category)
selectCategoryIdByItem itemId = do
  mCatId <- selectCategoryIdByItemMaybe itemId
  case mCatId of
    Nothing    -> throwError $ ItemNotFound itemId
    Just catId -> pure catId

-- | Get the 'CategoryRow' that an item belongs to.
--
-- Returns 'Nothing' if either the item or the category are not found.
selectCategoryRowByItemMaybe
  :: Uid Item -> ExceptT DatabaseError Transaction (Maybe CategoryRow)
selectCategoryRowByItemMaybe itemId = do
  catId <- selectCategoryIdByItemMaybe itemId
  join @Maybe <$> traverse selectCategoryRowMaybe catId

-- | Get a list of available categories' IDs.
--
-- Includes categories marked as deleted.
--
-- TODO explain why we store deleted categories at all.
selectCategoryIds :: ExceptT DatabaseError Transaction [Uid Category]
selectCategoryIds = do
  let statement :: Statement () [Uid Category]
      statement =
        rmap (map fromSingleColumn) $
        [queryRows|
          SELECT uid
          FROM categories
        |]
  lift $ HT.statement () statement

-- | Get all category rows.
selectCategoryRows :: ExceptT DatabaseError Transaction [CategoryRow]
selectCategoryRows = do
  catIds <- selectCategoryIds
  traverse selectCategoryRow catIds

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get an 'Item'.
--
-- Items marked as deleted will still be returned if they physically exist
-- in the database.
selectItemRowMaybe :: Uid Item -> ExceptT DatabaseError Transaction (Maybe ItemRow)
selectItemRowMaybe itemId = do
  let statement :: Statement (Uid Item) (Maybe ItemRow)
      statement = lmap SingleParam $
        [queryRowMaybe|
          SELECT
              uid
            , name
            , created
            , link
            , hackage
            , summary
            , ecosystem
            , notes
            , deleted
            , category_uid
            , pros_order
            , cons_order
          FROM items
          WHERE uid = $1
        |]
  lift $ HT.statement itemId statement

-- | Get an 'ItemRow'.
--
-- Items marked as deleted will still be returned if they physically exist
-- in the database.
--
-- Fails with 'ItemNotFound' when the item does not exist.
selectItemRow :: Uid Item -> ExceptT DatabaseError Transaction ItemRow
selectItemRow itemId = do
  mItemRow <- selectItemRowMaybe itemId
  case mItemRow of
    Nothing      -> throwError $ ItemNotFound itemId
    Just itemRow -> pure itemRow

-- | Get deleted ItemRows belonging to a category.
--
-- Returns item rows without order.
selectDeletedItemRowsByCategory :: Uid Category -> ExceptT DatabaseError Transaction [ItemRow]
selectDeletedItemRowsByCategory catId = do
  let statement :: Statement (Uid Category) [ItemRow]
      statement =
        lmap SingleParam $
        [queryRows|
          SELECT
              uid
            , name
            , created
            , link
            , hackage
            , summary
            , ecosystem
            , notes
            , deleted
            , category_uid
            , pros_order
            , cons_order
          FROM items
          WHERE category_uid = $1
            AND deleted = true
        |]
  lift $ HT.statement catId statement

-- | Get available ItemRows belonging to a category.
--
-- Returns item rows sorted by order.
selectItemRowsByCategory :: Uid Category -> ExceptT DatabaseError Transaction [ItemRow]
selectItemRowsByCategory catId = do
  itemUids <- categoryRowItemsOrder <$> selectCategoryRow catId
  traverse selectItemRow itemUids

-- | Get item id by trait.
selectItemIdByTraitMaybe :: Uid Trait -> ExceptT DatabaseError Transaction (Maybe (Uid Item))
selectItemIdByTraitMaybe traitId = do
  let statement :: Statement (Uid Trait) (Maybe (Uid Item))
      statement = dimap SingleParam (fmap fromSingleColumn) $
        [queryRowMaybe|
          SELECT item_uid
          FROM traits
          WHERE uid = $1
        |]
  lift $ HT.statement traitId statement

-- | Get item id by trait.
--
-- Can throw 'TraitNotFound'.
selectItemIdByTrait :: Uid Trait -> ExceptT DatabaseError Transaction (Uid Item)
selectItemIdByTrait traitId = do
  mItemId <- selectItemIdByTraitMaybe traitId
  case mItemId of
    Nothing     -> throwError $ TraitNotFound traitId
    Just itemId -> pure itemId

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get a 'TraitRow'.
--
-- Traits marked as deleted will still be returned if they physically exist
-- in the database.
selectTraitRowMaybe :: Uid Trait -> ExceptT DatabaseError Transaction (Maybe TraitRow)
selectTraitRowMaybe traitId = do
  let statement :: Statement (Uid Trait) (Maybe TraitRow)
      statement = lmap SingleParam $
        [queryRowMaybe|
          SELECT uid, content, deleted, type_, item_uid
          FROM traits
          WHERE uid = $1
        |]
  lift $ HT.statement traitId statement

-- | Get a 'TraitRow'.
--
-- Traits marked as deleted will still be returned if they physically exist
-- in the database.
--
-- Fails with 'TraitNotFound' when the trait does not exist.
selectTraitRow :: Uid Trait -> ExceptT DatabaseError Transaction TraitRow
selectTraitRow traitId = do
  mTraitRow <- selectTraitRowMaybe traitId
  case mTraitRow of
    Nothing       -> throwError $ TraitNotFound traitId
    Just traitRow -> pure traitRow

-- | Get deleted traits belonging to an item.
--
-- | To fetch pro and con traits use 'getDeletedTraitRowsByItem' twice.
selectDeletedTraitRowsByItem
  :: Uid Item
  -> TraitType
  -> ExceptT DatabaseError Transaction [TraitRow]
selectDeletedTraitRowsByItem itemId traitType = do
  let statement :: Statement (Uid Item, TraitType) [TraitRow]
      statement =
        [queryRows|
          SELECT uid, content, deleted, type_, item_uid
          FROM traits
          WHERE item_uid = $1
            AND deleted = true
            AND type_ = ($2 :: trait_type)
        |]
  lift $ HT.statement (itemId, traitType) statement

-- | Get available traits (they ordered) belonging to an item.
selectTraitRowsByItem
  :: Uid Item
  -> TraitType
  -> ExceptT DatabaseError Transaction [TraitRow]
selectTraitRowsByItem itemId traitType = do
  itemRow <- selectItemRow itemId
  let traitsOrder = case traitType of
        TraitTypePro -> itemRowProsOrder itemRow
        TraitTypeCon -> itemRowConsOrder itemRow
  traverse selectTraitRow traitsOrder
