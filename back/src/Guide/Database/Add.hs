{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

-- | Insert queries.
module Guide.Database.Add
       (
       -- * Trait
         addTrait
       -- * Item
       , addItem
       -- * Category
       , addCategory

       ) where

import Imports

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
import Guide.Database.Set (addItemIdToCategory, addTraitIdToItem)
import Guide.Database.Types
import Guide.Types.Core (Category (..), CategoryStatus (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


----------------------------------------------------------------------------
-- addCategory
----------------------------------------------------------------------------

-- | Insert category to database.
addCategory
  :: Uid Category       -- ^ New category's id
  -> "title" :! Text    -- ^ Title
  -> "group" :! Text    -- ^ Group
  -> UTCTime            -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addCategory catId (arg #title -> title) (arg #group -> group_) created = do
  let sql = [r|
        INSERT INTO categories
          ( uid
          , title
          , created
          , group_
          , status
          , notes
          , enabled_sections
          , items_order
          )
        VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
        |]
      encoder = categoryRowParams
      decoder = HD.noResult
  lift $ HT.statement
    CategoryRow
     { categoryRowUid = catId
     , categoryRowTitle = title
     , categoryRowCreated = created
     , categoryRowGroup = group_
     , categoryRowStatus = CategoryWIP
     , categoryRowNotes = ""
     , categoryRowSelections = Set.empty
     , categoryRowItemOrder = []
     }
    (Statement sql encoder decoder False)

----------------------------------------------------------------------------
-- addItem
----------------------------------------------------------------------------

-- | Insert item to database.
--
-- Item added to '_categoryItems' by default.
addItem
  :: Uid Category       -- ^ Category id
  -> Uid Item           -- ^ New item's id
  -> "name" :! Text     -- ^ Name
  -> UTCTime            -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addItem catId itemId (arg #name -> name) created = do
  let sql = [r|
        INSERT INTO items
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
        VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)
        |]
      encoder = itemRowParams
      decoder = HD.noResult
  lift $ HT.statement
    ItemRow
      { itemRowUid = itemId
      , itemRowName = name
      , itemRowCreated = created
      , itemRowLink = Nothing
      , itemRowHackage = Nothing
      , itemRowSummary = ""
      , itemRowEcosystem = ""
      , itemRowNotes = ""
      , itemRowDeleted = False
      , itemRowCategoryUid = catId
      , itemRowProsOrder = []
      , itemRowConsOrder = [] }
    (Statement sql encoder decoder False)
  -- Adds itemId to category's items_order list.
  addItemIdToCategory catId itemId

----------------------------------------------------------------------------
-- addTrait
----------------------------------------------------------------------------

-- | Insert trait to database.
--
-- Trait added to '_itemPros' or '_itemCons' by default.
addTrait
  :: Uid Item             -- ^ Item id
  -> Uid Trait            -- ^ New trait's id
  -> TraitType            -- ^ Pro or Con
  -> "content" :! Text    -- ^ Trait content
  -> ExceptT DatabaseError Transaction ()
addTrait itemId traitId traitType (arg #content -> content) = do
  let sql = [r|
        INSERT INTO traits (uid, content, deleted, type_, item_uid)
        VALUES ($1,$2,$3,($4 :: trait_type),$5)
        |]
      encoder = traitRowParams
      decoder = HD.noResult
  lift $ HT.statement
    TraitRow
      { traitRowUid = traitId
      , traitRowContent = content
      , traitRowDeleted  = False
      , traitRowType = traitType
      , traitRowItemUid = itemId
      }
    (Statement sql encoder decoder False)
  addTraitIdToItem itemId traitId (#traitType traitType)


-- Sandbox

-- Test add functions
testAdd :: IO ()
testAdd = do
  conn <- connect
  time <- getCurrentTime
  runTransactionExceptT conn Write (addCategory "category1111" (#title "addedCat") (#group "groupCat") time)
  cat <- runTransactionExceptT conn Read (getCategoryRow "category1111")
  print cat

  runTransactionExceptT conn Write (addItem "category1111" "item11112222" (#name "addedItem") time)
  item1 <- runTransactionExceptT conn Read (getItemRow "item11112222")
  print item1
  runTransactionExceptT conn Write (addItem "category1111" "item22223333" (#name "addedItem") time)
  item2 <- runTransactionExceptT conn Read (getItemRow "item22223333")
  print item2


  runTransactionExceptT conn Write (addTrait "item11112222" "traitPro1111" Pro (#content "content Pro 1"))
  traitP11 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitPro1111")
  print traitP11
  runTransactionExceptT conn Write (addTrait "item11112222" "traitPro1122" Pro (#content "content Pro 2"))
  traitP12 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitPro1122")
  print traitP12
  runTransactionExceptT conn Write (addTrait "item11112222" "traitCon1111" Con (#content "content Con 1"))
  traitC11 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitCon1111")
  print traitC11
  runTransactionExceptT conn Write (addTrait "item11112222" "traitCon1122" Con (#content "content Con 2"))
  traitC12 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitCon1122")
  print traitC12

  runTransactionExceptT conn Write (addTrait "item22223333" "traitPro2222" Pro (#content "content Pro 1"))
  traitP21 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitPro2222")
  print traitP21
  runTransactionExceptT conn Write (addTrait "item22223333" "traitPro2233" Pro (#content "content Pro 2"))
  traitP22 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitPro2233")
  print traitP22
  runTransactionExceptT conn Write (addTrait "item22223333" "traitCon2222" Con (#content "content Con 1"))
  traitC21 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitCon2222")
  print traitC21
  runTransactionExceptT conn Write (addTrait "item22223333" "traitCon2233" Con (#content "content Con 2"))
  traitC22 <- runTransactionExceptT conn Read (getTraitRowMaybe "traitCon2233")
  print traitC22
