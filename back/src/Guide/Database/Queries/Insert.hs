-- | Insert queries.
module Guide.Database.Queries.Insert
(
  -- * Category
  insertCategory,
  insertCategory2,
  insertCategoryWithCategory,
  insertCategoryWithCategoryRow,
  -- * Item
  insertItem,
  insertItemWithItemRow,
  -- * Trait
  insertTrait,
  insertTraitWithTraitRow,
)
where

import Imports

import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)

import qualified Hasql.Transaction as HT

import Guide.Database.Queries.Update
import Guide.Database.Types
import Guide.Database.Utils (execute)
import Guide.Markdown (toMarkdownBlock)
import Guide.Types.Core
import Guide.Uid


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

-- | Create category passing 'CategoryRow'.
insertCategoryWithCategoryRow :: CategoryRow -> ExceptT DatabaseError Transaction ()
insertCategoryWithCategoryRow categoryRow = do
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
  lift $ HT.statement categoryRow statement

insertCategory2
  :: Uid Category          -- ^ New category's id
  -> "title" :! Text
  -> "group" :! Text
  -> "created" :! UTCTime
  -> "status" :! CategoryStatus
  -> "enabledSections" :! Set ItemSection
  -> ExceptT DatabaseError Transaction ()
insertCategory2
  catId
  (arg #title -> title)
  (arg #group -> group_)
  (arg #created -> created)
  (arg #status -> status)
  (arg #enabledSections -> enabledSections)
  =
  do
  let category = Category
        { categoryUid = catId
        , categoryTitle = title
        , categoryCreated = created
        , categoryGroup = group_
        , categoryStatus = status
        , categoryNotes = toMarkdownBlock ""
        , categoryItems = []
        , categoryItemsDeleted = []
        , categoryEnabledSections = enabledSections
        }
  insertCategoryWithCategory category

-- | Create category passing 'Category'.
insertCategoryWithCategory :: Category -> ExceptT DatabaseError Transaction ()
insertCategoryWithCategory category = do
  let statement :: Statement (Uid Category, Category) ()
      statement =
        [execute|
          INSERT INTO categories
            ( uid
            , data
            )
          VALUES ($1,$2)
        |]
  lift $ HT.statement (categoryUid category, category) statement


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

-- | Create item passing 'ItemRow'.
insertItemWithItemRow :: ItemRow -> ExceptT DatabaseError Transaction ()
insertItemWithItemRow itemRow@ItemRow{..} = do
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
  lift $ HT.statement itemRow statement
  unless itemRowDeleted $ updateCategoryRow itemRowCategoryUid $
    _categoryRowItemsOrder %~ (++ [itemRowUid])

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

-- | Create trait record passing 'TraitRow'.
insertTraitWithTraitRow :: TraitRow -> ExceptT DatabaseError Transaction ()
insertTraitWithTraitRow traitRow@TraitRow{..} = do
  let statement :: Statement TraitRow ()
      statement =
        [execute|
          INSERT INTO traits (uid, content, deleted, type_, item_uid)
          VALUES ($1,$2,$3,($4 :: trait_type),$5)
        |]
  lift $ HT.statement traitRow statement
  unless traitRowDeleted $ case traitRowType of
    TraitTypePro ->
      updateItemRow traitRowItemUid $
        _itemRowProsOrder %~ (++ [traitRowUid])
    TraitTypeCon ->
      updateItemRow traitRowItemUid $
        _itemRowConsOrder %~ (++ [traitRowUid])
