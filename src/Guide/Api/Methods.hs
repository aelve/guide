{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Guide.Api.Methods where


import Imports

import Data.Acid as Acid
import Data.Text (Text)
import Servant

import Guide.Api.Types
import Guide.State
import Guide.Types
import Guide.Utils
import Guide.Cache

import qualified Data.Text as T
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

-- | Create a new category, given the title and the grandparent (aka group).
--
-- Returns the ID of the created category (or of the existing one if the
-- category with this title exists already).
createCategory :: DB -> Text -> Text -> Handler (Uid Category)
createCategory db title' group' = do
  -- If the category exists already, don't create it
  cats <- view categories <$> dbQuery db GetGlobalState
  let isDuplicate cat = T.toCaseFold (cat^.title) == T.toCaseFold title'
                     && T.toCaseFold (cat^.group_) == T.toCaseFold group'
  case find isDuplicate cats of
    Just c  -> return (c^.uid)
    Nothing -> do
      catId <- randomShortUid
      time <- liftIO getCurrentTime
      (_edit, _newCategory) <- dbUpdate db (AddCategory catId title' group' time)
      invalidateCache' db (CacheCategory catId)
      -- TODO addEdit edit
      return catId

-- | Delete a category.
deleteCategory :: DB -> Uid Category -> Handler NoContent
deleteCategory db catId = uncache db (CacheCategory catId) $ do
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
  invalidateCache' db (CacheItem itemId)
  -- TODO: addEdit edit
  pure itemId

-- TODO: move an item

-- | Delete an item.
deleteItem :: DB -> Uid Item -> Handler NoContent
deleteItem db itemId = uncache db (CacheItem itemId) $ do
  _mbEdit <- dbUpdate db (DeleteItem itemId)
  pure NoContent
  -- TODO: mapM_ addEdit mbEdit

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- TODO: move a trait

-- | Create a trait (pro/con).
setTrait :: DB -> Uid Item -> Uid Trait -> Text -> Handler NoContent
setTrait db itemId traitId text = do
    (_edit, _newItem) <- dbUpdate db (SetTraitContent itemId traitId text)
    pure NoContent

-- | Delete a trait (pro/con).
deleteTrait :: DB -> Uid Item -> Uid Trait -> Handler NoContent
deleteTrait db itemId traitId = uncache db (CacheItemTraits itemId) $ do
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

-- Twins of corresponding functions used in "Guide.Handlers".
-- TODO: remove them when the old backend is gone.

uncache :: (MonadIO m) => DB -> CacheKey -> m a -> m a
uncache db key act = do
  gs <- dbQuery db GetGlobalState
  x <- act
  invalidateCache gs key
  return x

invalidateCache' :: (MonadIO m) => DB -> CacheKey -> m ()
invalidateCache' db key = do
  gs <- dbQuery db GetGlobalState
  invalidateCache gs key
