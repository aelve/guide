-- | Insert queries.
module Guide.Database.Queries.Insert
(
  -- * Category
  insertCategory,
  -- * Item
  insertItem,
  -- * Trait
  insertTrait,
)
where

import Imports

import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Named

import qualified Hasql.Transaction as HT

import Guide.Database.Queries.Update
import Guide.Database.Types
import Guide.Database.Utils (execute)
import Guide.Types.Core
import Guide.Utils (Uid (..))


----------------------------------------------------------------------------
-- Category
----------------------------------------------------------------------------

-- | Create a category record.
insertCategory
  :: Uid Category          -- ^ New category's id
  -> "title" :! Text
  -> "group" :! Text
  -> "created" :! UTCTime
  -> "status" :! CategoryStatus
  -> "enabledSections" :! Set ItemSection
  -> ExceptT DatabaseError Transaction ()
insertCategory
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
-- Item
----------------------------------------------------------------------------

-- | Create an item record. The item will also be added to its category.
insertItem
  :: Uid Category          -- ^ Category id
  -> Uid Item              -- ^ New item's id
  -> "name" :! Text        -- ^ Name
  -> "created" :! UTCTime  -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
insertItem catId itemId (arg #name -> name) (arg #created -> created) = do
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
  updateCategoryRow catId $
    _categoryRowItemsOrder %~ (++ [itemId])

----------------------------------------------------------------------------
-- Trait
----------------------------------------------------------------------------

-- | Create a trait record. The trait will also be added to its item in the
-- pros or cons section.
insertTrait
  :: Uid Item             -- ^ Item id
  -> Uid Trait            -- ^ New trait's id
  -> TraitType            -- ^ Pro or Con
  -> "content" :! Text    -- ^ Trait content
  -> ExceptT DatabaseError Transaction ()
insertTrait itemId traitId traitType (arg #content -> content) = do
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
      updateItemRow itemId $
        _itemRowProsOrder %~ (++ [traitId])
    TraitTypeCon ->
      updateItemRow itemId $
        _itemRowConsOrder %~ (++ [traitId])
