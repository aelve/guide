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
         getTraitMaybe
       , getTraitsByItem
       -- * Item
       , getItem
       , getItemMaybe
       , getItemsByCategory
       -- * Category
       , getCategory
       , getCategoryMaybe
       , getCategories
       , getCategoryByItemMaybe
       , getCategoryIdByItemMaybe
       , getCategoryItemsOrder

       ) where

import Imports

import Contravariant.Extras.Contrazip (contrazip2, contrazip3)
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
import Guide.Markdown (toMarkdownBlock, toMarkdownInline, toMarkdownTree)
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get a 'Trait'.
--
-- Traits marked as deleted will still be returned if they physically exist
-- in the database.
getTraitMaybe :: Uid Trait -> ExceptT DatabaseError Transaction (Maybe Trait)
getTraitMaybe traitId = do
  let sql = [r|
        SELECT uid, content
        FROM traits
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe $ do
        _traitUid <- uidColumn
        _traitContent <- toMarkdownInline <$> textColumn
        pure $ Trait{..}
  lift $ HT.statement traitId (Statement sql encoder decoder False)

-- | Get traits belonging to an item.
--
-- The @#deleted@ flag specifies whether to return only "normal" or only
-- deleted traits. To get both, call 'getTraitsByItem' twice.
getTraitsByItem
  :: Uid Item
  -> "deleted" :! Bool
  -> TraitType
  -> ExceptT DatabaseError Transaction [Trait]
getTraitsByItem itemId (arg #deleted -> deleted) traitType = do
  let sql = [r|
        SELECT uid, content
        FROM traits
        WHERE item_uid = $1
          AND deleted = $2
          AND type_ = ($3 :: trait_type)
        |]
      encoder = contrazip3 uidParam boolParam traitTypeParam
      decoder = HD.rowList $ do
        _traitUid <- uidColumn
        _traitContent <- toMarkdownInline <$> textColumn
        pure $ Trait{..}
  lift $ HT.statement (itemId,deleted,traitType) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get an 'Item'.
--
-- Items marked as deleted will still be returned if they physically exist
-- in the database.
getItemMaybe :: Uid Item -> ExceptT DatabaseError Transaction (Maybe Item)
getItemMaybe itemId = do
  _itemPros <- getTraitsByItem itemId (#deleted False) Pro
  _itemProsDeleted <- getTraitsByItem itemId (#deleted True) Pro
  _itemCons <- getTraitsByItem itemId (#deleted False) Con
  _itemConsDeleted <- getTraitsByItem itemId (#deleted True) Con
  let prefix = "item-notes-" <> uidToText itemId <> "-"
  let sql = [r|
        SELECT uid, name, created, link, hackage, summary, ecosystem, notes
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe $ do
        _itemUid <- uidColumn
        _itemName <- textColumn
        _itemCreated <- timestamptzColumn
        _itemLink <- textColumnNullable
        _itemHackage <- textColumnNullable
        _itemSummary <- toMarkdownBlock <$> textColumn
        _itemEcosystem <- toMarkdownBlock <$> textColumn
        _itemNotes <- toMarkdownTree prefix <$> textColumn
        pure $ Item{..}
  lift $ HT.statement itemId (Statement sql encoder decoder False)

-- | Get an 'Item'.
--
-- Items marked as deleted will still be returned if they physically exist
-- in the database.
--
-- Fails with 'ItemNotFound' when the item does not exist.
getItem :: Uid Item -> ExceptT DatabaseError Transaction Item
getItem itemId = do
  mItem <- getItemMaybe itemId
  case mItem of
    Nothing   -> throwError $ ItemNotFound itemId
    Just item -> pure item

-- | Get items belonging to a category.
--
-- The @#deleted@ flag specifies whether to return only "normal" or only
-- deleted items. To get both, call 'getItemsByCategory' twice.
getItemsByCategory
  :: Uid Category
  -> "deleted" :! Bool
  -> ExceptT DatabaseError Transaction [Item]
getItemsByCategory catId (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid
        FROM items
        WHERE category_uid = $1
          AND deleted = $2
        |]
      encoder = contrazip2 uidParam boolParam
      decoder = HD.rowList $ uidColumn
  itemUids <- lift $ HT.statement (catId,deleted) (Statement sql encoder decoder False)
  traverse getItem itemUids

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get a 'Category'.
--
-- Categories marked as deleted will still be returned if they physically
-- exist in the database.
getCategoryMaybe :: Uid Category -> ExceptT DatabaseError Transaction (Maybe Category)
getCategoryMaybe catId = do
  _categoryItems <- getItemsByCategory catId (#deleted False)
  _categoryItemsDeleted <- getItemsByCategory catId (#deleted True)
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
        FROM categories
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe $ do
        _categoryUid <- uidColumn
        _categoryTitle <- textColumn
        _categoryCreated <- timestamptzColumn
        _categoryGroup_ <- textColumn
        _categoryStatus <- categoryStatusColumn
        _categoryNotes <- toMarkdownBlock <$> textColumn
        _categoryEnabledSections <- itemSectionSetColumn
        pure $ Category{..}
  lift $ HT.statement catId (Statement sql encoder decoder False)

-- | Get 'items_order' from category.
getCategoryItemsOrder :: Uid Category -> ExceptT DatabaseError Transaction [Uid Item]
getCategoryItemsOrder catId = do
  let sql = [r|
        SELECT items_order
        FROM categories
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.singleRow uidsColumn

  lift $ HT.statement catId (Statement sql encoder decoder False)

-- | Get a 'Category'.
--
-- Categories marked as deleted will still be returned if they physically
-- exist in the database.
--
-- Fails with 'CategoryNotFound' when the category does not exist.
getCategory :: Uid Category -> ExceptT DatabaseError Transaction Category
getCategory catId = do
  mCat <- getCategoryMaybe catId
  case mCat of
    Nothing  -> throwError $ CategoryNotFound catId
    Just cat -> pure cat
    -- TODO: consider not returning deleted categories? Otherwise somebody
    -- deletes a category but the page is still there.

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
      decoder = HD.rowMaybe $ uidColumn
  lift $ HT.statement itemId (Statement sql encoder decoder False)

-- | Get the category that an item belongs to.
--
-- Returns 'Nothing' if either the item or the category are not found.
getCategoryByItemMaybe
  :: Uid Item -> ExceptT DatabaseError Transaction (Maybe Category)
getCategoryByItemMaybe itemId = do
  catId <- getCategoryIdByItemMaybe itemId
  join @Maybe <$> traverse getCategoryMaybe catId

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
      decoder = HD.rowList $ uidColumn
  lift $ HT.statement () (Statement sql encoder decoder False)

-- | Get all categories.
--
-- Includes categories marked as deleted.
getCategories :: ExceptT DatabaseError Transaction [Category]
getCategories = do
  catIds <- getCategoryIds
  traverse getCategory catIds


-- Sandbox

-- | Just to test queries
getTest :: IO ()
getTest = do
  conn <- connect
  -- mTrait <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print mTrait
  -- traits <- runTransactionExceptT conn Read $
  --   getTraitsByItem "item11112222" (#deleted False) Pro
  -- print traits
  -- mItem <- runTransactionExceptT conn Read (getItemMaybe "item11112222")
  -- print mItem
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print item
  -- -- wrong uid
  -- -- itemErr <- runTransactionExceptT conn Read (getItemByItemId "wrong1234567")
  -- -- print itemErr
  -- items <- runTransactionExceptT conn Read $
  --   getItemsByCategory "category1111" (#deleted False)
  -- print items
  -- catM <- runTransactionExceptT conn Read (getCategoryMaybe "category1111")
  -- print catM
  cat <- runTransactionExceptT conn Read (getCategory "category1111")
  print cat
  -- mCatId <- runTransactionExceptT conn Read (getCategoryIdByItemMaybe "item11112222")
  -- print mCatId
  -- catIds <- runTransactionExceptT conn Read getCategoryIds
  -- print catIds
  -- cats <- runTransactionExceptT conn Read getCategories
  -- print cats
  itemOrder <- runTransactionExceptT conn Read (getCategoryItemsOrder "category1111")
  print itemOrder
