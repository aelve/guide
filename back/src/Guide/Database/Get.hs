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
import Hasql.Statement (Statement (..))
import Named
import Text.RawString.QQ (r)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS

import Guide.Database.Connection (connect, runDM)
import Guide.Database.Convert
import Guide.Markdown (toMarkdownBlock, toMarkdownInline, toMarkdownTree)
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))
import Guide.Database.Types


-- | Just to test queries
getTest :: IO ()
getTest = do
  conn <- connect
  mTrait <- runDM (getTraitMaybe "qwertassdf34") conn
  print mTrait
  traits <- runDM (getTraitsByItem "items1234567" (#deleted False) (#traitType Pro)) conn
  print traits
  mItem <- runDM (getItemMaybe "items1234567") conn
  print mItem
  item <- runDM (getItem "items1234567") conn
  print item
  -- wrong uid
  -- itemErr <- runDM (getItemByItemId "wrong1234567") conn
  -- print itemErr
  items <- runDM (getItemsByCategory "categories11" (#deleted False)) conn
  print items
  catM <- runDM (getCategoryMaybe "categories11") conn
  print catM
  cat <- runDM (getCategory "categories11") conn
  print cat
  catId <- runDM (getCategoryIdByItem "items1234567") conn
  print catId
  catIds <- runDM getCategoryIds conn
  print catIds
  cats <- runDM getCategories conn
  print cats

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get trait by id, either it deleted or not.
getTraitMaybe :: Uid Trait -> DatabaseMonad (Maybe Trait)
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
  toDatabaseMonad $ HS.statement traitId (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted and trait_type filters.
getTraitsByItem :: Uid Item -> "deleted" :! Bool -> "traitType" :! TraitType -> DatabaseMonad [Trait]
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
  toDatabaseMonad $ HS.statement (itemId,deleted,traitType) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get maybe item by id, either it deleted or not.
getItemMaybe :: Uid Item -> DatabaseMonad (Maybe Item)
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
  toDatabaseMonad $ HS.statement itemId (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItem :: Uid Item -> DatabaseMonad Item
getItem itemId = do
  mItem <- getItemMaybe itemId
  case mItem of
        Nothing   -> ExceptT $ pure $ Left $ ItemNotFound itemId
        Just item -> ExceptT $ pure $ Right item

-- | Get items with category id and deleted filters
getItemsByCategory :: Uid Category -> "deleted" :! Bool -> DatabaseMonad [Item]
getItemsByCategory catId (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid
        FROM items
        WHERE category_uid = $1
          AND deleted = $2
        |]
      encoder = contrazip2 uidParam boolParam
      decoder = HD.rowList $ uidColumn
  itemUids <- toDatabaseMonad $ HS.statement (catId,deleted) (Statement sql encoder decoder False)
  traverse getItem itemUids

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get maybe category by uid.
getCategoryMaybe :: Uid Category -> DatabaseMonad (Maybe Category)
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
  toDatabaseMonad $ HS.statement catId (Statement sql encoder decoder False)

-- | Get category by uid.
getCategory :: Uid Category -> DatabaseMonad Category
getCategory catId = do
  mCat <- getCategoryMaybe catId
  case mCat of
    Nothing  -> fail "getCategory returns nothing"
    Just cat -> pure cat

-- | Get category uid by item uid.
getCategoryIdByItem :: Uid Item -> DatabaseMonad (Uid Category)
getCategoryIdByItem itemId = do
  let sql = [r|
        SELECT category_uid
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.singleRow $ uidColumn
  toDatabaseMonad $ HS.statement itemId (Statement sql encoder decoder False)

-- | Get category by item uid.
getCategoryByItem :: Uid Item -> DatabaseMonad (Maybe Category)
getCategoryByItem itemId = do
  catId <- getCategoryIdByItem itemId
  getCategoryMaybe catId

-- | Get category's uid list
getCategoryIds :: DatabaseMonad [Uid Category]
getCategoryIds = do
  let sql = [r|
        SELECT uid
        FROM categories
        |]
      encoder = HE.noParams
      decoder = HD.rowList $ uidColumn
  toDatabaseMonad $ HS.statement () (Statement sql encoder decoder False)

-- | Get all categories
getCategories :: DatabaseMonad [Category]
getCategories = do
  catIds <- getCategoryIds
  traverse getCategory catIds
