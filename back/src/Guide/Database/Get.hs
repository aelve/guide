{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}


-- | Methods to select database's data.
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
       , getCategoryByItem

       ) where

import Imports

import Contravariant.Extras.Contrazip (contrazip2, contrazip3)
import Hasql.Session (Session)
import Hasql.Statement (Statement (..))
import Named
import Text.RawString.QQ (r)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS

import Guide.Database.Connection (connect, runSessionExceptT)
import Guide.Database.Convert
import Guide.Database.Types
import Guide.Markdown (toMarkdownBlock, toMarkdownInline, toMarkdownTree)
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


-- | Just to test queries
getTest :: IO ()
getTest = do
  conn <- connect
  mTrait <- runSessionExceptT (getTraitMaybe "qwertassdf34") conn
  print mTrait
  traits <- runSessionExceptT (getTraitsByItem "items1234567" (#deleted False) (#traitType Pro)) conn
  print traits
  mItem <- runSessionExceptT (getItemMaybe "items1234567") conn
  print mItem
  item <- runSessionExceptT (getItem "items1234567") conn
  print item
  -- wrong uid
  -- itemErr <- runSessionExceptT (getItemByItemId "wrong1234567") conn
  -- print itemErr
  items <- runSessionExceptT (getItemsByCategory "categories11" (#deleted False)) conn
  print items
  catM <- runSessionExceptT (getCategoryMaybe "categories11") conn
  print catM
  cat <- runSessionExceptT (getCategory "categories11") conn
  print cat
  catId <- runSessionExceptT (getCategoryIdByItem "items1234567") conn
  print catId
  catIds <- runSessionExceptT getCategoryIds conn
  print catIds
  cats <- runSessionExceptT getCategories conn
  print cats

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get trait by id, either it deleted or not.
getTraitMaybe :: Uid Trait -> ExceptT DatabaseError Session (Maybe Trait)
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
  lift $ HS.statement traitId (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted and trait_type filters.
getTraitsByItem
  :: Uid Item
  -> "deleted" :! Bool
  -> "traitType" :! TraitType
  -> ExceptT DatabaseError Session [Trait]
getTraitsByItem itemId (arg #deleted -> deleted) (arg #traitType -> traitType) = do
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
  lift $ HS.statement (itemId,deleted,traitType) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get maybe item by id, either it deleted or not.
getItemMaybe :: Uid Item -> ExceptT DatabaseError Session (Maybe Item)
getItemMaybe itemId = do
  _itemPros <- getTraitsByItem itemId (#deleted False) (#traitType Pro)
  _itemProsDeleted <- getTraitsByItem itemId (#deleted True) (#traitType Pro)
  _itemCons <- getTraitsByItem itemId (#deleted False) (#traitType Con)
  _itemConsDeleted <- getTraitsByItem itemId (#deleted True) (#traitType Con)
  let pref = "item-notes-" <> uidToText itemId <> "-"
  let sql = [r|
        SELECT uid, name, created, group_, link, hackage, summary, ecosystem, notes
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe $ do
        _itemUid <- uidColumn
        _itemName <- textColumn
        _itemCreated <- timestamptzColumn
        _itemGroup_ <- textColumnNullable
        _itemLink <- textColumnNullable
        _itemHackage <- textColumnNullable
        _itemSummary <- toMarkdownBlock <$> textColumn
        _itemEcosystem <- toMarkdownBlock <$> textColumn
        _itemNotes <- toMarkdownTree pref <$> textColumn
        pure $ Item{..}
  lift $ HS.statement itemId (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItem :: Uid Item -> ExceptT DatabaseError Session Item
getItem itemId = do
  mItem <- getItemMaybe itemId
  case mItem of
        Nothing   -> throwError $ ItemNotFound itemId
        Just item -> pure item

-- | Get items with category id and deleted filters
getItemsByCategory
  :: Uid Category
  -> "deleted" :! Bool
  -> ExceptT DatabaseError Session [Item]
getItemsByCategory catId (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid
        FROM items
        WHERE category_uid = $1
          AND deleted = $2
        |]
      encoder = contrazip2 uidParam boolParam
      decoder = HD.rowList $ uidColumn
  itemUids <- lift $ HS.statement (catId,deleted) (Statement sql encoder decoder False)
  traverse getItem itemUids

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get maybe category by uid.
getCategoryMaybe :: Uid Category -> ExceptT DatabaseError Session (Maybe Category)
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
        itemSelection <- itemSectionColumn
        let _categoryEnabledSections = Set.fromList itemSelection
        let _categoryGroups = Map.empty
        pure $ Category{..}
  lift $ HS.statement catId (Statement sql encoder decoder False)

-- | Get category by uid.
getCategory :: Uid Category -> ExceptT DatabaseError Session Category
getCategory catId = do
  mCat <- getCategoryMaybe catId
  case mCat of
    Nothing  -> throwError $ CategoryNotFound catId
    Just cat -> pure cat

-- | Get category uid by item uid.
getCategoryIdByItem :: Uid Item -> ExceptT DatabaseError Session (Uid Category)
getCategoryIdByItem itemId = do
  let sql = [r|
        SELECT category_uid
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.singleRow $ uidColumn
  lift $ HS.statement itemId (Statement sql encoder decoder False)

-- | Get category by item uid.
getCategoryByItem :: Uid Item -> ExceptT DatabaseError Session (Maybe Category)
getCategoryByItem itemId = do
  catId <- getCategoryIdByItem itemId
  getCategoryMaybe catId

-- | Get category's uid list
getCategoryIds :: ExceptT DatabaseError Session [Uid Category]
getCategoryIds = do
  let sql = [r|
        SELECT uid
        FROM categories
        |]
      encoder = HE.noParams
      decoder = HD.rowList $ uidColumn
  lift $ HS.statement () (Statement sql encoder decoder False)

-- | Get all categories
getCategories :: ExceptT DatabaseError Session [Category]
getCategories = do
  catIds <- getCategoryIds
  traverse getCategory catIds
