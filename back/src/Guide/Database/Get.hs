{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}


----------------------------------------------------------------------------
-- Methods to select database's data.
----------------------------------------------------------------------------
module Guide.Database.Get
       (
       -- Trait
         getTraitMaybe
       , getTraitsByItem
       -- Item
       , getItem
       , getItemMaybe
       , getItemsByCategory
       -- Category
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

import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS

import Guide.Database.Connection (connect, run')
import Guide.Database.Convert
import Guide.Types.Core (Category (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


-- | Just to test queries
getTest :: IO ()
getTest = do
  conn <- connect
  mTrait <- run' (getTraitMaybe "qwertassdf34") conn
  print mTrait
  traits <- run' (getTraitsByItem "items1234567" (#deleted False) (#traitType Pro)) conn
  print traits
  mItem <- run' (getItemMaybe "items1234567") conn
  print mItem
  item <- run' (getItem "items1234567") conn
  print item
  -- wrong uid
  -- itemErr <- run' (getItemByItemId "wrong1234567") conn
  -- print itemErr
  items <- run' (getItemsByCategory "categories11" (#deleted False)) conn
  print items
  catM <- run' (getCategoryMaybe "categories11") conn
  print catM
  cat <- run' (getCategory "categories11") conn
  print cat
  catId <- run' (getCategoryIdByItem "items1234567") conn
  print catId
  catIds <- run' getCategoryIds conn
  print catIds
  cats <- run' getCategories conn
  print cats

----------------------------------------------------------------------------
-- Get methods
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get trait by id, either it deleted or not.
getTraitMaybe :: Uid Trait -> Session (Maybe Trait)
getTraitMaybe traitId = do
  let sql = [r|
        SELECT uid, content
        FROM traits
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe traitRow
  HS.statement traitId (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted and trait_type filters.
getTraitsByItem :: Uid Item -> "deleted" :! Bool -> "traitType" :! TraitType -> Session [Trait]
getTraitsByItem itemId (arg #deleted -> deleted) (arg #traitType -> traitType) = do
  let sql = [r|
        SELECT uid, content
        FROM traits
        WHERE item_uid = $1
          AND deleted = $2
          AND type_ = ($3 :: trait_type)
        |]
      encoder = contrazip3 uidParam boolParam traitTypeParam
      decoder = HD.rowList traitRow
  HS.statement (itemId,deleted,traitType) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get maybe item by id, either it deleted or not.
getItemMaybe :: Uid Item -> Session (Maybe Item)
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
      decoder = HD.rowMaybe $
        itemRow pref _itemPros _itemProsDeleted _itemCons _itemConsDeleted
  HS.statement itemId (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItem :: Uid Item -> Session Item
getItem itemId = do
  mItem <- getItemMaybe itemId
  case mItem of
    Nothing   -> fail "getItemMaybe returns nothing"
    Just item -> pure item

-- | Get items with category id and deleted filters
getItemsByCategory :: Uid Category -> "deleted" :! Bool -> Session [Item]
getItemsByCategory catId (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid
        FROM items
        WHERE category_uid = $1
          AND deleted = $2
        |]
      encoder = contrazip2 uidParam boolParam
      decoder = HD.rowList $ uidColumn
  itemUids <- HS.statement (catId,deleted) (Statement sql encoder decoder False)
  traverse getItem itemUids

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get maybe category by uid.
getCategoryMaybe :: Uid Category -> Session (Maybe Category)
getCategoryMaybe catId = do
  _categoryItems <- getItemsByCategory catId (#deleted False)
  _categoryItemsDeleted <- getItemsByCategory catId (#deleted True)
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
        FROM categories
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.rowMaybe $ categoryRow _categoryItems _categoryItemsDeleted
  HS.statement catId (Statement sql encoder decoder False)

-- | Get category by uid.
getCategory :: Uid Category -> Session Category
getCategory catId = do
  _categoryItems <- getItemsByCategory catId (#deleted False)
  _categoryItemsDeleted <- getItemsByCategory catId (#deleted True)
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
        FROM categories
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.singleRow $ categoryRow _categoryItems _categoryItemsDeleted
  HS.statement catId (Statement sql encoder decoder False)

-- | Get category uid by item uid.
getCategoryIdByItem :: Uid Item -> Session (Uid Category)
getCategoryIdByItem itemId = do
  let sql = [r|
        SELECT category_uid
        FROM items
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.singleRow $ uidColumn
  HS.statement itemId (Statement sql encoder decoder False)

-- | Get category by item uid.
getCategoryByItem :: Uid Item -> Session (Maybe Category)
getCategoryByItem itemId = do
  catId <- getCategoryIdByItem itemId
  getCategoryMaybe catId

-- | Get category's uid list
getCategoryIds :: Session [Uid Category]
getCategoryIds = do
  let sql = [r|
        SELECT uid
        FROM categories
        |]
      encoder = HE.noParams
      decoder = HD.rowList $ uidColumn
  HS.statement () (Statement sql encoder decoder False)

-- | Get all categories
getCategories :: Session [Category]
getCategories = do
  catIds <- getCategoryIds
  traverse getCategory catIds
