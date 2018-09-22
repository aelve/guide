{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Guide.Api.Methods where


import Imports

import Servant
import Data.Acid as Acid
import qualified Data.Text as T
import Data.Text (Text)

import Guide.Types
import Guide.State
import Guide.Utils
import Guide.Api.Types
import qualified Guide.Search as Search

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get a list of available categories.
getCategories :: DB -> Handler [CCategoryInfo]
getCategories db = do
  dbQuery db GetCategories <&> \xs ->
    map toCCategoryInfo xs

-- | Get a single category and all of its items.
getCategory :: DB -> Uid Category -> Handler CCategoryFull
getCategory db catId =
  dbQuery db (GetCategoryMaybe catId) >>= \case
    Nothing  -> throwError err404
    Just cat -> pure (toCCategoryFull cat)

-- | Create a new category, given the title.
--
-- Returns the ID of the created category (or of the existing one if the
-- category with this title exists already).
createCategory :: DB -> Text -> Handler (Uid Category)
createCategory db title' = do
  -- If the category exists already, don't create it
  cats <- view categories <$> dbQuery db GetGlobalState
  let hasSameTitle cat = T.toCaseFold (cat^.title) == T.toCaseFold title'
  case find hasSameTitle cats of
    Just c  -> return (c^.uid)
    Nothing -> do
      catId <- randomShortUid
      time <- liftIO getCurrentTime
      (_edit, newCategory) <- dbUpdate db (AddCategory catId title' time)
      -- TODO addEdit edit
      return (newCategory^.uid)

-- | Delete a category.
deleteCategory :: DB -> Uid Category -> Handler NoContent
deleteCategory db catId = do
  _mbEdit <- dbUpdate db (DeleteCategory catId)
  pure NoContent
  -- TODO mapM_ addEdit mbEdit

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Create a new item, given the name.
--
-- Returns the ID of the created item. Unlike 'createCategory', allows items
-- with duplicated names.
createItem :: DB -> Uid Category -> Text -> Handler (Uid Item)
createItem db catId name' = do
  -- TODO: do something if the category doesn't exist (e.g. has been
  -- already deleted)
  itemId <- randomShortUid
  -- If the item name looks like a Hackage library, assume it's a Hackage
  -- library.
  let isAllowedChar c = isAscii c && (isAlphaNum c || c == '-')
      looksLikeLibrary = T.all isAllowedChar name'
      kind' = if looksLikeLibrary then Library (Just name') else Other
  time <- liftIO getCurrentTime
  (_edit, _newItem) <- dbUpdate db (AddItem catId itemId name' time kind')
  -- TODO: addEdit edit
  pure itemId

-- TODO: move an item

-- | Delete an item.
deleteItem :: DB -> Uid Item -> Handler NoContent
deleteItem db itemId = do
  _mbEdit <- dbUpdate db (DeleteItem itemId)
  pure NoContent
  -- TODO: mapM_ addEdit mbEdit

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- TODO: move a trait

-- | Delete a trait (pro/con).
deleteTrait :: DB -> Uid Item -> Uid Trait -> Handler NoContent
deleteTrait db itemId traitId = do
  _mbEdit <- dbUpdate db (DeleteTrait itemId traitId)
  pure NoContent
  -- TODO: mapM_ addEdit mbEdit

----------------------------------------------------------------------------
-- Search
----------------------------------------------------------------------------

-- | Site-wide search.
--
-- Returns at most 100 results.
search :: DB -> Text -> Handler [CSearchResult]
search db searchQuery = do
  gs <- dbQuery db GetGlobalState
  pure $ map toCSearchResult $ take 100 $ Search.search searchQuery gs

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Update something in the database.
dbUpdate :: (MonadIO m, EventState event ~ GlobalState, UpdateEvent event)
         => DB -> event -> m (EventResult event)
dbUpdate db x = liftIO $ do
  Acid.update db SetDirty
  Acid.update db x

-- | Read something from the database.
dbQuery :: (MonadIO m, EventState event ~ GlobalState, QueryEvent event)
        => DB -> event -> m (EventResult event)
dbQuery db x = liftIO $
  Acid.query db x
