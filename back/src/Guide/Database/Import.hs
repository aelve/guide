{-# LANGUAGE FlexibleContexts #-}

-- | Module contains all stuff to migrate from AcidState to Postgres.
module Guide.Database.Import
       (
        loadIntoPostgres
       ) where

import Imports

import Data.Acid (EventResult, EventState, QueryEvent, query)
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (Mode (..))
import Data.Generics.Uniplate.Operations (Biplate, transformBi)

import Guide.Database.Connection
import Guide.Database.Queries.Insert
import Guide.Database.Queries.Select
import Guide.Database.Schema
import Guide.Database.Types
import Guide.State
import Guide.Types.Core
import Guide.Config
import Guide.Logger


-- | Load categories and archives categories from acid state to postgres
-- and check if they are equal.
--
-- NOTE: It loads categories and categoriesDeleted fields of GlobalState only.
loadIntoPostgres :: Config -> IO ()
loadIntoPostgres config@Config{..} = withLogger config $ \logger -> do
  withDB (pure ()) $ \db -> do
    globalState <- dbQuery logger db GetGlobalState
    postgresLoader logger globalState

postgresLoader :: Logger -> GlobalState -> IO ()
postgresLoader logger globalState = do
    -- Postgres should be started and 'guide' base created.
    -- Don't forget to drop and create schema
    {-
      DROP SCHEMA public CASCADE;
      CREATE SCHEMA public;
      GRANT ALL ON SCHEMA public TO postgres;
      GRANT ALL ON SCHEMA public TO public;
    -}
    setupDatabase
    -- Upload to Postgres
    conn <- connect
    runTransactionExceptT conn Write $ insertCategories globalState
    -- Download from Postgres
    catPostgres <- runTransactionExceptT conn Read $
      selectCategories (#archived False)
    catarchivedPostgres <- runTransactionExceptT conn Read $
      selectCategories (#archived True)
    -- Check identity of available categories
    let checkedCat =
          sortOn categoryUid catPostgres ==
          sortOn categoryUid (categories globalState)
    -- Check identity of archived categories
    let checkedCatDeleted =
          sortOn categoryUid catarchivedPostgres ==
          sortOn categoryUid (categoriesDeleted globalState)

    let checked = checkedCat && checkedCatDeleted
    logDebugIO logger $ format "AcidState == Postgres: {}" checked
    unless checked exitFailure
  where
    -- Insert all categories from AcidState either archived or not.
    -- Categories be normilised before insertion. See 'normalizeUTC'.
    insertCategories :: GlobalState -> ExceptT DatabaseError Transaction ()
    insertCategories GlobalState{..} = do
      mapM_ (insertCategoryWithCategory (#archived False) . normalizeUTC)
        categories
      mapM_ (insertCategoryWithCategory (#archived True) . normalizeUTC)
        categoriesDeleted

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Read something from the database.
dbQuery :: (EventState event ~ GlobalState, QueryEvent event, Show event)
        => Logger -> DB -> event -> IO (EventResult event)
dbQuery logger db x = do
  logDebugIO logger $ "dbQuery: " +|| x ||+ ""
  liftIO $ query db x

-- | Format recursivly all UTCTime fields of Category to Postgres way.
normalizeUTC :: Biplate Category UTCTime => Category -> Category
normalizeUTC = transformBi cutUTCTime

-- | Truncate pico up to 6 digits.
-- | Haskell UTCTime '2019-08-22 09:03:45.736488657 UTC' becomes
-- | Postgres timestamptz '2019-08-22 09:03:45.736488 UTC'.
cutUTCTime :: UTCTime -> UTCTime
cutUTCTime UTCTime{..} = UTCTime{utctDay, utctDayTime = utctDayTimeCut}
  where
    utctDayTimeCut = picosecondsToDiffTime pico12Cut
    pico12 = diffTimeToPicoseconds utctDayTime
    pico12Cut = truncate ((fromInteger pico12 / 1000000) :: Double) * 1000000

----------------------------------------------------------------------------
-- Insert helpers
----------------------------------------------------------------------------

-- | Insert category at whole (with items and traits).
-- insertCategoryWhole
--   :: "deleted" :! Bool
--   -> Category
--   -> ExceptT DatabaseError Transaction ()
-- insertCategoryWhole (arg #deleted -> deleted) category@Category{..} = do
--   insertCategoryByRow category (#deleted deleted)
--   insertItemsOfCategory category
--   mapM_ insertTraitsOfItem categoryItems
--   mapM_ insertTraitsOfItem categoryItemsDeleted

-- | Insert to postgres all items of Category.
-- insertItemsOfCategory :: Category -> ExceptT DatabaseError Transaction ()
-- insertItemsOfCategory Category{..} = do
--   mapM_ (insertItemByRow categoryUid (#deleted False)) categoryItems
--   mapM_ (insertItemByRow categoryUid (#deleted True)) categoryItemsDeleted

-- | Insert to postgres all traits of Item.
-- insertTraitsOfItem :: Item -> ExceptT DatabaseError Transaction ()
-- insertTraitsOfItem Item{..} = do
--   mapM_ (insertTraitByRow itemUid (#deleted False) TraitTypePro) itemPros
--   mapM_ (insertTraitByRow itemUid (#deleted True) TraitTypePro) itemProsDeleted
--   mapM_ (insertTraitByRow itemUid (#deleted False) TraitTypeCon) itemCons
--   mapM_ (insertTraitByRow itemUid (#deleted True) TraitTypeCon) itemConsDeleted

-- | Insert category passing 'Category'.
-- insertCategoryByRow
--   :: Category
--   -> "deleted" :! Bool
--   -> ExceptT DatabaseError Transaction ()
-- insertCategoryByRow category (arg #deleted -> deleted) = do
--     let categoryRow = categoryToRowCategory category (#deleted deleted)
--     insertCategoryWithCategoryRow categoryRow

-- | Insert item passing 'Item'.
-- insertItemByRow
--   :: Uid Category
--   -> "deleted" :! Bool
--   -> Item
--   -> ExceptT DatabaseError Transaction ()
-- insertItemByRow catId (arg #deleted -> deleted) item = do
--   let itemRow = itemToRowItem catId (#deleted deleted) item
--   insertItemWithItemRow itemRow

-- | Insert trait passing 'Trait'.
-- insertTraitByRow
--   :: Uid Item
--   -> "deleted" :! Bool
--   -> TraitType
--   -> Trait
--   -> ExceptT DatabaseError Transaction ()
-- insertTraitByRow itemId (arg #deleted -> deleted) traitType trait = do
--   let traitRow = traitToTraitRow itemId (#deleted deleted) traitType trait
--   insertTraitWithTraitRow traitRow

----------------------------------------------------------------------------
-- Get helpers
----------------------------------------------------------------------------

-- | Get all categories and categoriesDeleted.
-- getCategories :: ExceptT DatabaseError Transaction ([Category], [Category])
-- getCategories = do
--     categoryRowsAll <- selectCategoryRows
--     let (categoryRowsDeleted, categoryRows) =
--           partition categoryRowDeleted categoryRowsAll
--     categories <- traverse getCategoryByRow categoryRows
--     categoriesDeleted <- traverse getCategoryByRow categoryRowsDeleted
--     pure (categories, categoriesDeleted)

-- | Get category by CategoryRow
-- getCategoryByRow :: CategoryRow -> ExceptT DatabaseError Transaction Category
-- getCategoryByRow categoryRow@CategoryRow{..} = do
--   itemRows <- selectItemRowsByCategory categoryRowUid
--   items <- traverse getItemByRow itemRows
--   itemRowsDeleted <- selectDeletedItemRowsByCategory categoryRowUid
--   itemsDeleted <- traverse getItemByRow itemRowsDeleted
--   pure $ categoryRowToCategory (#items items)
--     (#itemsDeleted itemsDeleted) categoryRow

-- | Get Item by ItemRow
-- getItemByRow :: ItemRow -> ExceptT DatabaseError Transaction Item
-- getItemByRow itemRow@ItemRow{..} = do
--   proTraitRows <- selectTraitRowsByItem itemRowUid TraitTypePro
--   let proTraits = map traitRowToTrait proTraitRows
--   proDeletedTraitRows <- selectDeletedTraitRowsByItem itemRowUid TraitTypePro
--   let proDeletedTraits = map traitRowToTrait proDeletedTraitRows
--   conTraitRows <- selectTraitRowsByItem itemRowUid TraitTypeCon
--   let conTraits = map traitRowToTrait conTraitRows
--   conDeletedTraitRows <- selectDeletedTraitRowsByItem itemRowUid TraitTypeCon
--   let conDeletedTraits = map traitRowToTrait conDeletedTraitRows
--   pure $ itemRowToItem
--     (#proTraits proTraits)
--     (#proDeletedTraits proDeletedTraits)
--     (#conTraits conTraits)
--     (#conDeletedTraits conDeletedTraits)
--     itemRow
