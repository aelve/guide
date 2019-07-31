{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

-- | Update queries.
module Guide.Database.Set
       (
       -- * Set
       -- ** Trait
         setTraitContent
       , setTraitDeleted
       -- ** Item
       , setItemName
       , setItemLink
       , setItemHackage
       , setItemSummary
       , setItemNotes
       , setItemEcosystem
       , setItemDeleted
       -- ** Category
       , setCategoryTitle
       , setCategoryGroup
       , setCategoryNotes
       , setCategoryStatus
       , setCategoryEnabledSections
       , addItemIdToCategory

       -- * Deletion
       , deleteCategory
       , deleteItem
       , deleteTrait

       -- * Moving
       , moveItem

       ) where

import Imports

import Contravariant.Extras.Contrazip (contrazip2)
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (Mode (..))
import Named
import Text.RawString.QQ (r)

import qualified Data.Set as Set
import qualified Hasql.Decoders as HD
import qualified Hasql.Transaction as HT

import Guide.Database.Connection (connect, runTransactionExceptT)
import Guide.Database.Convert
import Guide.Database.Get
import Guide.Database.Types
import Guide.Types.Core (Category (..), CategoryStatus (..), Item (..), ItemSection (..),
                         Trait (..))
import Guide.Utils (Direction (..), Uid (..), Url, moveDown, moveUp)


----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Set category title.
setCategoryTitle :: Uid Category -> "title" :! Text -> ExceptT DatabaseError Transaction ()
setCategoryTitle catId (arg #title -> title) = do
  let sql = [r|
        UPDATE categories
        SET title = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (catId, title) (Statement sql encoder decoder False)

-- | Set category group.
setCategoryGroup :: Uid Category -> "group_" :! Text -> ExceptT DatabaseError Transaction ()
setCategoryGroup catId (arg #group_ -> group_) = do
  let sql = [r|
        UPDATE categories
        SET group_ = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (catId, group_) (Statement sql encoder decoder False)

-- | Set category notes.
setCategoryNotes :: Uid Category -> "notes" :! Text -> ExceptT DatabaseError Transaction ()
setCategoryNotes catId (arg #notes -> notes) = do
  let sql = [r|
        UPDATE categories
        SET notes = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (catId, notes) (Statement sql encoder decoder False)

-- | Set new category notes.
setCategoryStatus :: Uid Category -> CategoryStatus -> ExceptT DatabaseError Transaction ()
setCategoryStatus catId status = do
  let sql = [r|
        UPDATE categories
        SET status_ = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam categoryStatusParam
      decoder = HD.noResult
  lift $ HT.statement (catId, status) (Statement sql encoder decoder False)

-- | Set category selections.
setCategoryEnabledSections
  :: Uid Category
  -> Set ItemSection     -- ^ Sections to enable
  -> Set ItemSection     -- ^ Sections to disable
  -> ExceptT DatabaseError Transaction ()
setCategoryEnabledSections catId toEnable toDisable = do
  oldSections <- _categoryEnabledSections <$> getCategory catId
  let newSections = (oldSections <> toEnable) Set.\\ toDisable
  let sql = [r|
        UPDATE categories
        SET enabled_sections = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam itemSectionSetParam
      decoder = HD.noResult
  lift $ HT.statement (catId, newSections) (Statement sql encoder decoder False)

-- | Add itemId to category items_order.
addItemIdToCategory :: Uid Category -> Uid Item -> ExceptT DatabaseError Transaction ()
addItemIdToCategory catId itemId = do
    itemsOrder <- getCategoryItemsOrder catId
    let addItemId =
          if notElem itemId itemsOrder then itemId : itemsOrder else itemsOrder
    let sql = [r|
          UPDATE categories
          SET items_order = $2
          WHERE uid = $1
          |]
        encoder = contrazip2 uidParam uidsParam
        decoder = HD.noResult
    lift $ HT.statement
      (catId, addItemId) (Statement sql encoder decoder False)

-- | Delete itemId from category items_order.
deleteItemIdFromCategory :: Uid Category -> Uid Item -> ExceptT DatabaseError Transaction ()
deleteItemIdFromCategory catId itemId = do
    itemsOrder <- getCategoryItemsOrder catId
    let deleteItemId =
          if elem itemId itemsOrder then filter (/= itemId) itemsOrder else itemsOrder
    let sql = [r|
          UPDATE categories
          SET items_order = $2
          WHERE uid = $1
          |]
        encoder = contrazip2 uidParam uidsParam
        decoder = HD.noResult
    lift $ HT.statement
      (catId, deleteItemId) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Set item name.
setItemName :: Uid Item -> "name" :! Text -> ExceptT DatabaseError Transaction ()
setItemName itemId (arg #name -> name) = do
  let sql = [r|
        UPDATE items
        SET name = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (itemId, name) (Statement sql encoder decoder False)

-- | Set item link.
setItemLink :: Uid Item -> "link" :? Url -> ExceptT DatabaseError Transaction ()
setItemLink itemId (argF #link -> link) = do
  let sql = [r|
        UPDATE items
        SET link = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParamNullable
      decoder = HD.noResult
  lift $ HT.statement (itemId, link) (Statement sql encoder decoder False)

-- | Set item hackage.
--
-- "hackage" :? Text == Maybe Text with label.
setItemHackage :: Uid Item -> "hackage" :? Text -> ExceptT DatabaseError Transaction ()
setItemHackage itemId (argF #hackage -> hackage) = do
  let sql = [r|
        UPDATE items
        SET hackage = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParamNullable
      decoder = HD.noResult
  lift $ HT.statement (itemId, hackage) (Statement sql encoder decoder False)

-- | Set item summary.
setItemSummary :: Uid Item -> "summary" :! Text -> ExceptT DatabaseError Transaction ()
setItemSummary itemId (arg #summary -> summary) = do
  let sql = [r|
        UPDATE items
        SET summary = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (itemId, summary) (Statement sql encoder decoder False)

-- | Set item notes.
setItemNotes :: Uid Item -> "notes" :! Text -> ExceptT DatabaseError Transaction ()
setItemNotes itemId (arg #notes -> notes) = do
  let sql = [r|
        UPDATE items
        SET notes = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (itemId, notes) (Statement sql encoder decoder False)

-- | Set item ecosystem.
setItemEcosystem :: Uid Item -> "ecosystem" :! Text -> ExceptT DatabaseError Transaction ()
setItemEcosystem itemId (arg #ecosystem -> ecosystem) = do
  let sql = [r|
        UPDATE items
        SET ecosystem = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (itemId, ecosystem) (Statement sql encoder decoder False)

-- | Set item to be deleted.
--
-- When 'deleted' is True, item will marked as deleted (_categoryItemsDeleted)
-- otherwise item will marked as notDeleted (_categoryItems)
setItemDeleted :: Uid Item -> "deleted" :! Bool -> ExceptT DatabaseError Transaction ()
setItemDeleted itemId (arg #deleted -> deleted) = do
  mCategoryId <- getCategoryIdByItemMaybe itemId
  case mCategoryId of
    Nothing -> throwError ItemOrCategoryNotFound
    Just catId -> do
      let sql = [r|
            UPDATE items
            SET deleted = $2
            WHERE uid = $1
            |]
          encoder = contrazip2 uidParam boolParam
          decoder = HD.noResult
      lift $ HT.statement (itemId, deleted) (Statement sql encoder decoder False)
      if deleted then deleteItemIdFromCategory catId itemId
      else addItemIdToCategory catId itemId

-- | Move item up or down.
moveItem :: Uid Item -> "direction" :! Bool -> ExceptT DatabaseError Transaction ()
moveItem itemId (arg #direction -> direction) = do
  let move = if direction then moveUp else moveDown
  mCategoryId <- getCategoryIdByItemMaybe itemId
  case mCategoryId of
    Nothing -> throwError ItemOrCategoryNotFound
    Just catId -> do
      itemsOrder <- getCategoryItemsOrder catId
      let newItemsOrder = move (== itemId) itemsOrder
      let sql = [r|
            UPDATE categories
            SET items_order = $2
            WHERE uid = $1
            |]
          encoder = contrazip2 uidParam uidsParam
          decoder = HD.noResult
      lift $ HT.statement (catId, newItemsOrder) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Set trait content.
setTraitContent :: Uid Trait -> "content" :! Text -> ExceptT DatabaseError Transaction ()
setTraitContent traitId (arg #content -> content) = do
  let sql = [r|
        UPDATE traits
        SET content = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (traitId, content) (Statement sql encoder decoder False)

-- | Set trait to be deleted.
--
-- When 'deleted' is True, trait will marked as deleted.
setTraitDeleted :: Uid Trait -> "deleted" :! Bool -> ExceptT DatabaseError Transaction ()
setTraitDeleted traitId (arg #deleted -> deleted) = do
  let sql = [r|
        UPDATE traits
        SET deleted = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam boolParam
      decoder = HD.noResult
  lift $ HT.statement (traitId, deleted) (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- Deletion
----------------------------------------------------------------------------

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

-- | Delete item completly.
deleteItem :: Uid Item -> ExceptT DatabaseError Transaction ()
deleteItem itemId = do
  mCategoryId <- getCategoryIdByItemMaybe itemId
  case mCategoryId of
    Nothing -> throwError ItemOrCategoryNotFound
    Just catId -> do
      let sql = [r|
            DELETE FROM items
            WHERE uid = $1
            |]
          encoder = uidParam
          decoder = HD.noResult
      lift $ HT.statement itemId (Statement sql encoder decoder False)
      -- it need to delete itemId from category's items_order
      deleteItemIdFromCategory catId itemId

-- | Delete trait completly.
deleteTrait :: Uid Trait -> ExceptT DatabaseError Transaction ()
deleteTrait traitId = do
  let sql = [r|
        DELETE FROM traits
        WHERE uid = $1
        |]
      encoder = uidParam
      decoder = HD.noResult
  lift $ HT.statement traitId (Statement sql encoder decoder False)


-- Sandbox

-- Test add functions
testSet :: IO ()
testSet = do
  conn <- connect
  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryTitle cat
  -- runTransactionExceptT conn Write (setCategoryTitle "category1111" "addedCatNew")
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryTitle cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryGroup_ cat
  -- runTransactionExceptT conn Write (setCategoryGroup "category1111" "groupNew2")
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryGroup_ cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryNotes cat
  -- runTransactionExceptT conn Write (setCategoryNotes "category1111" "new note")
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryNotes cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryStatus cat
  -- runTransactionExceptT conn Write (setCategoryStatus "category1111" CategoryStub)
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryStatus cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryEnabledSections cat
  -- runTransactionExceptT conn Write
  --   (setCategoryEnabledSections "category1111"
  --     (Set.fromList [ItemProsConsSection, ItemNotesSection])
  --     (Set.fromList [ItemEcosystemSection]))
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryEnabledSections cat'

  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemName item
  -- runTransactionExceptT conn Write (setItemName "item11112222" (#name "new note"))
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemName item'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemLink item
  -- runTransactionExceptT conn Write (setItemLink "item11112222" (#link "ya.ru"))
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemLink item'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemLink item
  -- runTransactionExceptT conn Write (setItemLink "item11112222" ! defaults)
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemLink item'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemHackage item
  -- runTransactionExceptT conn Write (setItemHackage "item11112222" ! #hackage "hello")
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemHackage item'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemSummary item
  -- runTransactionExceptT conn Write (setItemSummary "item11112222" ! #summary "hello, people!")
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemSummary item'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemNotes item
  -- runTransactionExceptT conn Write (setItemNotes "item11112222" ! #notes "hello, people with notes!")
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemNotes item'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemEcosystem item
  -- runTransactionExceptT conn Write (setItemEcosystem "item11112222" ! #ecosystem "Export ENV")
  -- item' <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print $ _itemEcosystem item'

  -- Mark item 'deleted' and check it if it deleted from items_order list
  -- itemOrder <- runTransactionExceptT conn Read (getCategoryItemsOrder "category1111")
  -- print itemOrder
  -- runTransactionExceptT conn Write (setItemDeleted "item11112222" ! #deleted False)
  -- itemOrder' <- runTransactionExceptT conn Read (getCategoryItemsOrder "category1111")
  -- print itemOrder'

  -- trait <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print trait
  -- runTransactionExceptT conn Write (setTraitContent "trait1112222" ! #content "all pro")
  -- trait' <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print trait'

  -- trait <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print trait
  -- runTransactionExceptT conn Write (setTraitDeleted "trait1112222" ! #deleted False)
  -- trait' <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print trait'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print item

  -- Delete trait
  -- trait <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print trait
  -- runTransactionExceptT conn Write (deleteTrait "trait1112222")
  -- trait' <- runTransactionExceptT conn Read (getTraitMaybe "trait1112222")
  -- print trait'
  -- item <- runTransactionExceptT conn Read (getItem "item11112222")
  -- print item

  -- Delete item
  item <- runTransactionExceptT conn Read (getItemMaybe "item11112222")
  print item
  runTransactionExceptT conn Write (deleteItem "item11112222")
  item' <- runTransactionExceptT conn Read (getItemMaybe "item11112222")
  print item'

  -- Delete Category
  -- cat <- runTransactionExceptT conn Read (getCategoryMaybe "category1111")
  -- print cat
  -- runTransactionExceptT conn Write (deleteCategory "category1111")
  -- cat' <- runTransactionExceptT conn Read (getCategoryMaybe "category1111")
  -- print cat'

  -- Move Item
  -- cat' <- runTransactionExceptT conn Read (getCategoryMaybe "category1111")
  -- print cat'
  itemOrder <- runTransactionExceptT conn Read (getCategoryItemsOrder "category1111")
  print itemOrder
  -- runTransactionExceptT conn Write (moveItem "item22223333" ! #direction False)
  itemOrder' <- runTransactionExceptT conn Read (getCategoryItemsOrder "category1111")
  print itemOrder'
