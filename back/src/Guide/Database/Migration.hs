-- | Module contains all stuff to migrate from AcidState to Postgres.
module Guide.Database.Migration
       (
        loadIntoPostgres
       ) where

import Imports

import Data.Acid (EventResult, EventState, QueryEvent, query)
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (Mode (..))
import Named
import Data.Generics.Uniplate.Data (transformBis, transformer)

import Guide.Database.Connection
import Guide.Database.Queries.Insert
import Guide.Database.Queries.Select
import Guide.Database.Schema
import Guide.Database.Types
import Guide.State
import Guide.Types.Core
import Guide.Utils (Uid (..))
import Guide.Config
import Guide.Logger


-- | Load categories and deleted categories from acid state to postgres
-- and check if they are equal.
--
-- NOTE: It loads categories and categoriesDeleted fields of GlobalState only.
loadIntoPostgres :: Config -> IO ()
loadIntoPostgres config@Config{..} = withLogger config $ \logger -> do
  withDB (pure ()) $ \db -> do
    globalState@GlobalState{..} <- dbQuery logger db GetGlobalState
    postgresLoader logger globalState

postgresLoader :: Logger -> GlobalState -> IO ()
postgresLoader logger globalState@GlobalState{..} = do
    -- Postgres should be started and 'guide' base created.
    -- Use docker, for example: https://www.notion.so/aelve/Postgres-26db590f88734f6d83a5318d17b6188e
    setupDatabase

    -- Upload to Postgres
    conn <- connect
    runTransactionExceptT conn Write $ insertCategories globalState

    -- Download from Postgres
    (categoriesPostgres, categoriesDeletedPostgres) <- runTransactionExceptT conn Read getCategories

    -- Prepare data
    let catNorm = map normalizeUTC categories
    let catDeletedNorm = map normalizeUTC categoriesDeleted

    -- Check equality of availiable categories
    let checkCatLength = length categoriesPostgres == length catNorm
    let checkCat = sort categoriesPostgres == sort catNorm

    -- Check equality of deleted categories
    let checkCatDeletedLength = length categoriesDeletedPostgres == length catDeletedNorm
    let checkCatDeleted = sort categoriesDeletedPostgres == sort catDeletedNorm

    logDebugIO logger $ format "Categories length: {}" checkCatLength
    logDebugIO logger $ format "Categories equality: {}" checkCat
    logDebugIO logger $ format "Deleted categories length: {}" checkCatDeletedLength
    logDebugIO logger $ format "Deleted categories equality: {}" checkCatDeleted

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Read something from the database.
dbQuery :: (EventState event ~ GlobalState, QueryEvent event, Show event)
        => Logger -> DB -> event -> IO (EventResult event)
dbQuery logger db x = do
  logDebugIO logger $ "dbQuery: " +|| x ||+ ""
  liftIO $ query db x

-- normalizeUTC :: GlobalState -> GlobalState
normalizeUTC :: Category -> Category
normalizeUTC = transformBis [[transformer cutUTCTime]]

cutUTCTime :: UTCTime -> UTCTime
cutUTCTime UTCTime{..} = UTCTime{utctDay, utctDayTime = utctDayTimeCut}
  where
    utctDayTimeCut = picosecondsToDiffTime pico12Cut
    pico12 = diffTimeToPicoseconds utctDayTime
    pico12Cut = floor ((fromInteger pico12 / 1000000) :: Double) * 1000000

----------------------------------------------------------------------------
-- Insert helpers
----------------------------------------------------------------------------

-- | Insert all categories from AcidState either deleted or not.
insertCategories :: GlobalState -> ExceptT DatabaseError Transaction ()
insertCategories GlobalState{..} = do
  mapM_ (insertCategoryWhole (#deleted False)) categories
  mapM_ (insertCategoryWhole (#deleted True)) categoriesDeleted

-- | Insert category at whole (with items and traits).
insertCategoryWhole
  :: "deleted" :! Bool
  -> Category
  -> ExceptT DatabaseError Transaction ()
insertCategoryWhole (arg #deleted -> deleted) category@Category{..} = do
  insertCategoryF category (#deleted deleted)
  insertItemFromCategory category
  mapM_ insertTraitsFromItem categoryItems
  mapM_ insertTraitsFromItem categoryItemsDeleted

-- | Insert to postgres all items from Category.
insertItemFromCategory :: Category -> ExceptT DatabaseError Transaction ()
insertItemFromCategory Category{..} = do
  mapM_ (insertItemF categoryUid (#deleted False)) categoryItems
  mapM_ (insertItemF categoryUid (#deleted True)) categoryItemsDeleted

-- | Insert to postgres all traits from Item.
insertTraitsFromItem :: Item -> ExceptT DatabaseError Transaction ()
insertTraitsFromItem Item{..} = do
  mapM_ (insertFullTraitF itemUid (#deleted False) TraitTypePro) itemPros
  mapM_ (insertFullTraitF itemUid (#deleted True) TraitTypePro) itemProsDeleted
  mapM_ (insertFullTraitF itemUid (#deleted False) TraitTypeCon) itemCons
  mapM_ (insertFullTraitF itemUid (#deleted True) TraitTypeCon) itemConsDeleted

-- | Insert category passing 'Category'.
insertCategoryF
  :: Category
  -> "deleted" :! Bool
  -> ExceptT DatabaseError Transaction ()
insertCategoryF category (arg #deleted -> deleted) = do
    let categoryRow = categoryToRowCategory category (#deleted deleted)
    insertCategoryWithCategoryRow categoryRow

-- | Insert item passing 'Item'.
insertItemF
  :: Uid Category
  -> "deleted" :! Bool
  -> Item
  -> ExceptT DatabaseError Transaction ()
insertItemF catId (arg #deleted -> deleted) item = do
  let itemRow = itemToRowItem catId (#deleted deleted) item
  insertItemWithItemRow itemRow

-- | Insert trait passing 'Trait'.
insertFullTraitF
  :: Uid Item
  -> "deleted" :! Bool
  -> TraitType
  -> Trait
  -> ExceptT DatabaseError Transaction ()
insertFullTraitF itemId (arg #deleted -> deleted) traitType trait = do
  let traitRow = traitToTraitRow itemId (#deleted deleted) traitType trait
  insertTraitWithTraitRow traitRow

----------------------------------------------------------------------------
-- Get helpers
----------------------------------------------------------------------------

-- | Get all categories and categoriesDeleted.
getCategories :: ExceptT DatabaseError Transaction ([Category], [Category])
getCategories = do
    categoryRowsAll <- selectCategoryRows
    let (categoryRowsDeleted, categoryRows) = partition categoryRowDeleted categoryRowsAll
    categories <- traverse getCategoryByRow categoryRows
    categoriesDeleted <- traverse getCategoryByRow categoryRowsDeleted
    pure (categories, categoriesDeleted)

-- | Get category by CategoryRow
getCategoryByRow :: CategoryRow -> ExceptT DatabaseError Transaction Category
getCategoryByRow categoryRow@CategoryRow{..} = do
  itemRows <- selectItemRowsByCategory categoryRowUid
  items <- traverse getItemByRow itemRows
  itemRowsDeleted <- selectDeletedItemRowsByCategory categoryRowUid
  itemsDeleted <- traverse getItemByRow itemRowsDeleted
  pure $ categoryRowToCategory (#items items) (#itemsDeleted itemsDeleted) categoryRow

-- | Get Item by ItemRow
getItemByRow :: ItemRow -> ExceptT DatabaseError Transaction Item
getItemByRow itemRow@ItemRow{..} = do
  proTraitRows <- selectTraitRowsByItem itemRowUid TraitTypePro
  let proTraits = map traitRowToTrait proTraitRows
  proDeletedTraitRows <- selectDeletedTraitRowsByItem itemRowUid TraitTypePro
  let proDeletedTraits = map traitRowToTrait proDeletedTraitRows
  conTraitRows <- selectTraitRowsByItem itemRowUid TraitTypeCon
  let conTraits = map traitRowToTrait conTraitRows
  conDeletedTraitRows <- selectDeletedTraitRowsByItem itemRowUid TraitTypeCon
  let conDeletedTraits = map traitRowToTrait conDeletedTraitRows
  pure $ itemRowToItem (#proTraits proTraits) (#proDeletedTraits proDeletedTraits)
    (#conTraits conTraits) (#conDeletedTraits conDeletedTraits) itemRow

