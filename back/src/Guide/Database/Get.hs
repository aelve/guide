{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}


-- | Read-only database queries.
module Guide.Database.Get
       (
       -- * Trait
         getTraitRowMaybe
       , getTraitRow
       , getTraitRowsByItem
       , getDeletedTraitRowsByItem
       -- * Item
       , getItemRow
       , getItemRowMaybe
       , getItemRowsByCategory
       , getDeletedItemRowsByCategory
       , getItemIdByTrait
       , getItemIdByTraitMaybe
       -- * Category
       , getCategoryRow
       , getCategoryRowMaybe
       , getCategoryRows
       , getCategoryRowByItemMaybe
       , getCategoryIdByItem
       , getCategoryIdByItemMaybe

       ) where

import Imports

import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Data.Profunctor (lmap, rmap, dimap)

import qualified Hasql.Transaction as HT

import Guide.Database.Types
import Guide.Database.Utils
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get a 'TraitRow'.
--
-- Traits marked as deleted will still be returned if they physically exist
-- in the database.
getTraitRowMaybe :: Uid Trait -> ExceptT DatabaseError Transaction (Maybe TraitRow)
getTraitRowMaybe traitId = do
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
getTraitRow :: Uid Trait -> ExceptT DatabaseError Transaction TraitRow
getTraitRow traitId = do
  mTraitRow <- getTraitRowMaybe traitId
  case mTraitRow of
    Nothing       -> throwError $ TraitNotFound traitId
    Just traitRow -> pure traitRow

-- | Get deleted traits belonging to an item.
--
-- | To fetch pro and con traits use 'getDeletedTraitRowsByItem' twice.
getDeletedTraitRowsByItem
  :: Uid Item
  -> TraitType
  -> ExceptT DatabaseError Transaction [TraitRow]
getDeletedTraitRowsByItem itemId traitType = do
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
getTraitRowsByItem
  :: Uid Item
  -> TraitType
  -> ExceptT DatabaseError Transaction [TraitRow]
getTraitRowsByItem itemId traitType = do
  itemRow <- getItemRow itemId
  let traitsOrder = case traitType of
        TraitTypePro -> itemRowProsOrder itemRow
        TraitTypeCon -> itemRowConsOrder itemRow
  traverse getTraitRow traitsOrder

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get an 'Item'.
--
-- Items marked as deleted will still be returned if they physically exist
-- in the database.
getItemRowMaybe :: Uid Item -> ExceptT DatabaseError Transaction (Maybe ItemRow)
getItemRowMaybe itemId = do
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
getItemRow :: Uid Item -> ExceptT DatabaseError Transaction ItemRow
getItemRow itemId = do
  mItemRow <- getItemRowMaybe itemId
  case mItemRow of
    Nothing      -> throwError $ ItemNotFound itemId
    Just itemRow -> pure itemRow

-- | Get deleted ItemRows belonging to a category.
--
-- Returns item rows without order.
getDeletedItemRowsByCategory :: Uid Category -> ExceptT DatabaseError Transaction [ItemRow]
getDeletedItemRowsByCategory catId = do
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

-- Get available ItemRows belonging to a category.
--
-- Returns item rows sorted by order.
getItemRowsByCategory :: Uid Category -> ExceptT DatabaseError Transaction [ItemRow]
getItemRowsByCategory catId = do
  itemUids <- categoryRowItemsOrder <$> getCategoryRow catId
  traverse getItemRow itemUids

-- | Get item id by trait.
getItemIdByTraitMaybe :: Uid Trait -> ExceptT DatabaseError Transaction (Maybe (Uid Item))
getItemIdByTraitMaybe traitId = do
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
getItemIdByTrait :: Uid Trait -> ExceptT DatabaseError Transaction (Uid Item)
getItemIdByTrait traitId = do
  mItemId <- getItemIdByTraitMaybe traitId
  case mItemId of
    Nothing     -> throwError $ TraitNotFound traitId
    Just itemId -> pure itemId

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get a 'CategoryRow'.
getCategoryRowMaybe :: Uid Category -> ExceptT DatabaseError Transaction (Maybe CategoryRow)
getCategoryRowMaybe catId = do
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
getCategoryRow :: Uid Category -> ExceptT DatabaseError Transaction CategoryRow
getCategoryRow catId = do
  mCatRow <- getCategoryRowMaybe catId
  case mCatRow of
    Nothing     -> throwError $ CategoryNotFound catId
    Just catRow -> pure catRow

-- | Get the ID of the category that an item belongs to.
getCategoryIdByItemMaybe
  :: Uid Item -> ExceptT DatabaseError Transaction (Maybe (Uid Category))
getCategoryIdByItemMaybe itemId = do
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
getCategoryIdByItem :: Uid Item -> ExceptT DatabaseError Transaction (Uid Category)
getCategoryIdByItem itemId = do
  mCatId <- getCategoryIdByItemMaybe itemId
  case mCatId of
    Nothing    -> throwError $ ItemNotFound itemId
    Just catId -> pure catId

-- | Get the 'CategoryRow' that an item belongs to.
--
-- Returns 'Nothing' if either the item or the category are not found.
getCategoryRowByItemMaybe
  :: Uid Item -> ExceptT DatabaseError Transaction (Maybe CategoryRow)
getCategoryRowByItemMaybe itemId = do
  catId <- getCategoryIdByItemMaybe itemId
  join @Maybe <$> traverse getCategoryRowMaybe catId

-- | Get a list of available categories' IDs.
--
-- Includes categories marked as deleted.
--
-- TODO explain why we store deleted categories at all.
getCategoryIds :: ExceptT DatabaseError Transaction [Uid Category]
getCategoryIds = do
  let statement :: Statement () [Uid Category]
      statement =
        rmap (map fromSingleColumn) $
        [queryRows|
          SELECT uid
          FROM categories
        |]
  lift $ HT.statement () statement

-- | Get all category rows.
getCategoryRows :: ExceptT DatabaseError Transaction [CategoryRow]
getCategoryRows = do
  catIds <- getCategoryIds
  traverse getCategoryRow catIds
