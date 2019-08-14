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
import Named

import qualified Hasql.Transaction as HT

import Guide.Database.Set
import Guide.Database.Types
import Guide.Database.Utils (execute)
import Guide.Types.Core (Category (..), CategoryStatus (..), Item (..), Trait (..), TraitType (..), ItemSection)
import Guide.Utils (Uid (..))


----------------------------------------------------------------------------
-- addCategory
----------------------------------------------------------------------------

-- | Insert category to database.
addCategory
  :: Uid Category          -- ^ New category's id
  -> "title" :! Text
  -> "group" :! Text
  -> "created" :! UTCTime
  -> "status" :! CategoryStatus
  -> "enabledSections" :! Set ItemSection
  -> ExceptT DatabaseError Transaction ()
addCategory
  catId
  (arg #title -> title)
  (arg #group -> group_)
  (arg #created -> created)
  (arg #status -> status)
  (arg #enabledSections -> enabledSections)
  =
  do
  let statement :: Statement CategoryRow ()
      statement =
        [execute|
          INSERT INTO categories
            ( uid
            , title
            , created
            , group_
            , status
            , notes
            , enabled_sections
            , items_order
            , deleted
            )
          VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)
        |]
  lift $ HT.statement
    CategoryRow
      { categoryRowUid = catId
      , categoryRowTitle = title
      , categoryRowCreated = created
      , categoryRowGroup = group_
      , categoryRowStatus = status
      , categoryRowNotes = ""
      , categoryRowEnabledSections = enabledSections
      , categoryRowItemsOrder = []
      , categoryRowDeleted = False
      }
    statement

----------------------------------------------------------------------------
-- addItem
----------------------------------------------------------------------------

-- | Insert item to database.
--
-- Item added to '_categoryItems' by default.
addItem
  :: Uid Category          -- ^ Category id
  -> Uid Item              -- ^ New item's id
  -> "name" :! Text        -- ^ Name
  -> "created" :! UTCTime  -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addItem catId itemId (arg #name -> name) (arg #created -> created) = do
  let statement :: Statement ItemRow ()
      statement =
        [execute|
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
      , itemRowConsOrder = []
      }
    statement
  modifyCategoryRow catId $
    _categoryRowItemsOrder %~ (++ [itemId])

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
  let statement :: Statement TraitRow ()
      statement =
        [execute|
          INSERT INTO traits (uid, content, deleted, type_, item_uid)
          VALUES ($1,$2,$3,($4 :: trait_type),$5)
        |]
  lift $ HT.statement
    TraitRow
      { traitRowUid = traitId
      , traitRowContent = content
      , traitRowDeleted = False
      , traitRowType = traitType
      , traitRowItemUid = itemId
      }
    statement
  case traitType of
    TraitTypePro ->
      modifyItemRow itemId $
        _itemRowProsOrder %~ (++ [traitId])
    TraitTypeCon ->
      modifyItemRow itemId $
        _itemRowConsOrder %~ (++ [traitId])
