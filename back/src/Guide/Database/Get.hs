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

import Contravariant.Extras.Contrazip (contrazip2)
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (Mode (Read))
import Named
import Text.RawString.QQ (r)

import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Transaction as HT

import Guide.Database.Connection (connect, runTransactionExceptT)
import Guide.Database.Convert
import Guide.Database.Types
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
  let sql = [r|
        SELECT uid, content, deleted, type_, item_uid
        FROM traits
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe traitRowColumn
  lift $ HT.statement traitId (Statement sql encoder decoder False)

-- | Get an 'TraitRow'.
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
-- | To fetch pro and con traits use 'getDeletedTraitRowsByItem twice.
getDeletedTraitRowsByItem
  :: Uid Item
  -> "traitType" :! TraitType
  -> ExceptT DatabaseError Transaction [TraitRow]
getDeletedTraitRowsByItem itemId (arg #traitType -> traitType) = do
  let sql = [r|
        SELECT uid, content, deleted, type_, item_uid
        FROM traits
        WHERE item_uid = $1
          AND deleted = True
          AND type_ = ($2 :: trait_type)
        |]
      encoder = contrazip2 uidParam traitTypeParam
      decoder = HD.rowList traitRowColumn
  lift $ HT.statement (itemId, traitType) (Statement sql encoder decoder False)

-- | Get available traits (they ordered) beloning to an item.
getTraitRowsByItem
  :: Uid Item
  -> "traitType" :! TraitType
  -> ExceptT DatabaseError Transaction [TraitRow]
getTraitRowsByItem itemId (arg #traitType -> traitType) = do
  itemRow <- getItemRow itemId
  let traitsOrder = case traitType of
        Pro -> itemRowProsOrder itemRow
        Con -> itemRowConsOrder itemRow
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
  let sql = [r|
        SELECT
          ( uid
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
          )
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe itemRowColumn
  lift $ HT.statement itemId (Statement sql encoder decoder False)

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
  let sql = [r|
        SELECT
          ( uid
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
          )
        FROM items
        WHERE category_uid = $1
          AND deleted = True
        |]
      encoder = uidParam
      decoder = HD.rowList itemRowColumn
  lift $ HT.statement catId (Statement sql encoder decoder False)

-- Get available ItemRows belonging to a category.
--
-- Returns item rows sorted by order.
getItemRowsByCategory :: Uid Category -> ExceptT DatabaseError Transaction [ItemRow]
getItemRowsByCategory catId = do
  itemUids <- categoryRowItemOrder <$> getCategoryRow catId
  traverse getItemRow itemUids

-- | Get item id by trait.
getItemIdByTraitMaybe :: Uid Trait -> ExceptT DatabaseError Transaction (Maybe (Uid Item))
getItemIdByTraitMaybe traitId = do
  let sql = [r|
        SELECT item_uid
        FROM traits
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe uidColumn
  lift $ HT.statement traitId (Statement sql encoder decoder False)

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
  let sql = [r|
        SELECT
          ( uid
          , title
          , created
          , group_
          , status
          , notes
          , enabled_sections
          , items_order
          )
        FROM categories
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe categoryRowColumn
  lift $ HT.statement catId (Statement sql encoder decoder False)

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
  let sql = [r|
        SELECT category_uid
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe uidColumn
  lift $ HT.statement itemId (Statement sql encoder decoder False)

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
  let sql = [r|
        SELECT uid
        FROM categories
        |]
      encoder = HE.noParams
      decoder = HD.rowList uidColumn
  lift $ HT.statement () (Statement sql encoder decoder False)

-- | Get all category rows.
getCategoryRows :: ExceptT DatabaseError Transaction [CategoryRow]
getCategoryRows = do
  catIds <- getCategoryIds
  traverse getCategoryRow catIds


-- Sandbox

-- | Just to test queries
getTest :: IO ()
getTest = do
  conn <- connect
  -- mTrait <- runTransactionExceptT conn Read (getTraitRowMaybe "trait1112222")
  -- print mTrait
  -- traits <- runTransactionExceptT conn Read $
  --   getTraitRowsByItem "item11112222" (#deleted False) Pro
  -- print traits
  -- mItem <- runTransactionExceptT conn Read (getItemRowMaybe "item11112222")
  -- print mItem
  -- item <- runTransactionExceptT conn Read (getItemRow "item11112222")
  -- print item
  -- -- wrong uid
  -- -- itemErr <- runTransactionExceptT conn Read (getItemByItemId "wrong1234567")
  -- -- print itemErr
  -- items <- runTransactionExceptT conn Read $
  --   getItemsByCategory "category1111" (#deleted False)
  -- print items
  -- catM <- runTransactionExceptT conn Read (getCategoryRowMaybe "category1111")
  -- print catM
  cat <- runTransactionExceptT conn Read (getCategoryRow "category1111")
  print cat
  -- mCatId <- runTransactionExceptT conn Read (getCategoryIdByItemMaybe "item11112222")
  -- print mCatId
  -- catIds <- runTransactionExceptT conn Read getCategoryIds
  -- print catIds
  -- cats <- runTransactionExceptT conn Read getCategoryRows
  -- print cats
