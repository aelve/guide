{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


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

import Guide.Database.Connection
import Guide.Database.Queries.Insert
import Guide.Database.Schema
import Guide.Database.Types
-- import Guide.Logger (logDebug)
import Guide.State
import Guide.Types.Core
import Guide.Utils (Uid (..))


-- | Load categories and deleted categories from acid state to postgres
-- and check if they are equal.
loadIntoPostgres :: IO ()
loadIntoPostgres = do
  withDB (pure ()) $ \db -> do
    globalState <- dbQuery db GetGlobalState
    -- Load to Postgres
    conn <- connect
    runTransactionExceptT conn Write $ insertCategories globalState
    -- Check equality

-- | Read something from the database.
dbQuery :: (EventState event ~ GlobalState, QueryEvent event, Show event)
        => DB -> event -> IO (EventResult event)
dbQuery db x = do
  -- logDebug $ "dbQuery: " +|| x ||+ ""
  liftIO $ query db x

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
