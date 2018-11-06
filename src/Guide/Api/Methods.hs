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
import Guide.Api.Utils
import Guide.State
import Guide.Types
import Guide.Utils

import qualified Data.Set as S
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
  when (T.null title') $ do throwError (err400 {errBody = "Title not provided"})
  when (T.null group') $ do throwError (err400 {errBody = "Group' not provided"})
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
      -- TODO addEdit edit
      return catId

-- | Edit categoty's note.
setCategoryNotes :: DB -> Uid Category -> Text -> Handler NoContent
setCategoryNotes db catId note =
  dbQuery db (GetCategoryMaybe catId) >>= \case
    Nothing -> throwError (err404 {errBody = "Category not found"})
    Just _ -> do
      (_edit, _newCategory) <- dbUpdate db (SetCategoryNotes catId note)
      -- TODO diff and merge
      pure NoContent

-- | Edit category's info (title, group, status, sections (pro/con, ecosystem, note)).
setCategoryInfo :: DB -> Uid Category -> CCategoryInfoEdit -> Handler NoContent
setCategoryInfo db catId CCategoryInfoEdit{..} =
  dbQuery db (GetCategoryMaybe catId) >>= \case
    Nothing -> throwError (err404 {errBody = "Category not found"})
    Just category -> do
      -- TODO diff and merge
      _ <- dbUpdate db $ SetCategoryTitle catId $ unH ccieTitle
      _ <- dbUpdate db $ SetCategoryGroup catId $ unH ccieGroup
      _ <- dbUpdate db $ SetCategoryStatus catId $ unH ccieStatus
      let oldEnabledSections = category ^. enabledSections
      let newEnabledSections = unH ccieSections
      _ <- dbUpdate db $ ChangeCategoryEnabledSections catId
          (newEnabledSections S.\\ oldEnabledSections)
          (oldEnabledSections S.\\ newEnabledSections)
      -- TODO record edits
      pure NoContent

-- | Delete a category.
deleteCategory :: DB -> Uid Category -> Handler NoContent
deleteCategory db catId =
  dbQuery db (GetCategoryMaybe catId) >>= \case
    Nothing -> throwError (err404 {errBody = "Category not found"})
    Just _ -> do
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
createItem db catId name' =
  dbQuery db (GetCategoryMaybe catId) >>= \case
    Nothing -> throwError (err404 {errBody = "Category not found"})
    Just _ -> do
      if T.null name' then throwError (err400 {errBody = "Name not provided"})
      else do
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

-- | Set item's info
setItemInfo :: DB -> Uid Item -> CItemInfo -> Handler NoContent
setItemInfo db itemId CItemInfo{..} =
  dbQuery db (GetItemMaybe itemId) >>= \case
    Nothing -> throwError (err404 {errBody = "Item not found"})
    Just _ -> do
      -- TODO diff and merge
      _ <- dbUpdate db $ SetItemName itemId $ unH ciiName
      _ <- dbUpdate db $ SetItemGroup itemId $ unH ciiGroup
      _ <- dbUpdate db $ SetItemLink itemId $ unH ciiLink
      _ <- dbUpdate db $ SetItemKind itemId $ unH ciiKind
      pure NoContent

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

-- | Create a trait (pro/con).
createTrait :: DB -> Uid Item -> TraitType -> Text -> Handler (Uid Trait)
createTrait db itemId traitType text = do
  when (T.null text) $ throwError (err400 {errBody = "Trait text not provided"})
  traitId <- randomShortUid
  (_edit, _newTrait) <- case traitType of
    Con -> dbUpdate db (AddCon itemId traitId text)
    Pro -> dbUpdate db (AddPro itemId traitId text)
  -- TODO: mapM_ addEdit mbEdit
  pure traitId

-- | Update the text of a trait (pro/con).
setTrait :: DB -> Uid Item -> Uid Trait -> Text -> Handler NoContent
setTrait db itemId traitId text = do
  (_edit, _newTrait) <- dbUpdate db (SetTraitContent itemId traitId text)
  -- TODO diff and merge
  pure NoContent

-- | Delete a trait (pro/con).
deleteTrait :: DB -> Uid Item -> Uid Trait -> Handler NoContent
deleteTrait db itemId traitId = do
  _mbEdit <- dbUpdate db (DeleteTrait itemId traitId)
  pure NoContent


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
