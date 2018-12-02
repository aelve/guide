{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Guide.Api.Methods where


import Imports

import Data.Acid as Acid
import Data.Aeson (encode)
import Data.Text (Text)
import Servant

import Guide.Api.Guider (Guider)
import Guide.Api.Types
import Guide.Api.Utils
import Guide.Config (Config (..))
import Guide.Diff (merge)
import Guide.Markdown (MarkdownBlock (..), MarkdownInline (..), MarkdownTree (..))
import Guide.State
import Guide.Types
import Guide.Utils
import Data.IP (IP)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Guide.Search as Search

----------------------------------------------------------------------------
-- Categories
----------------------------------------------------------------------------

-- | Get a list of available categories.
getCategories :: DB -> Guider [CCategoryInfo]
getCategories db = do
  dbQuery db GetCategories <&> \xs ->
    map toCCategoryInfo xs

-- | Get a single category and all of its items.
getCategory :: DB -> Uid Category -> Guider CCategoryFull
getCategory db catId = toCCategoryFull <$> getCategoryOrFail db catId

-- | Create a new category, given the title and the grandparent (aka group).
--
-- Returns the ID of the created category (or of the existing one if the
-- category with this title exists already).
createCategory :: DB -> RequestDetails -> Text -> Text -> Guider (Uid Category)
createCategory db requestDetails title' group' = do
  when (T.null title') $ throwError err400{errBody = "Title not provided"}
  when (T.null group') $ throwError err400{errBody = "Group' not provided"}
  -- If the category exists already, don't create it
  cats <- view categories <$> dbQuery db GetGlobalState
  let isDuplicate cat = T.toCaseFold (cat^.title) == T.toCaseFold title'
        && T.toCaseFold (cat^.group_) == T.toCaseFold group'
  case find isDuplicate cats of
    Just c  -> return (c^.uid)
    Nothing -> do
      catId <- randomShortUid
      time <- liftIO getCurrentTime
      (_edit, _) <- dbUpdate db (AddCategory catId title' group' time)
      config <- ask
      addEdit db config requestDetails _edit
      return catId

-- | Edit categoty's note.
setCategoryNotes :: DB -> RequestDetails -> Uid Category -> CTextEdit -> Guider NoContent
setCategoryNotes db requestDetails catId CTextEdit{..} = do
  serverModified <- markdownBlockMdSource . _categoryNotes <$> getCategoryOrFail db catId
  checkConflict CTextEdit{..} serverModified
  (_edit, _) <- dbUpdate db (SetCategoryNotes catId $ unH cteModified)
  config <- ask
  addEdit db config requestDetails _edit
  pure NoContent

-- | Edit category's info (title, group, status, sections (pro/con, ecosystem, note)).
setCategoryInfo :: DB -> RequestDetails -> Uid Category -> CCategoryInfoEdit -> Guider NoContent
setCategoryInfo db requestDetails catId CCategoryInfoEdit{..} = do
  category <- getCategoryOrFail db catId
  -- TODO diff and merge
  (_editTitle, _) <- dbUpdate db $ SetCategoryTitle catId $ unH ccieTitle
  (_editGroup, _) <- dbUpdate db $ SetCategoryGroup catId $ unH ccieGroup
  (_editStatus, _) <- dbUpdate db $ SetCategoryStatus catId $ unH ccieStatus
  let oldEnabledSections = category ^. enabledSections
  let newEnabledSections = unH ccieSections
  (_editSection, _) <- dbUpdate db $ ChangeCategoryEnabledSections catId
      (newEnabledSections S.\\ oldEnabledSections)
      (oldEnabledSections S.\\ newEnabledSections)
  config <- ask
  mapM_ (addEdit db config requestDetails) [_editTitle, _editGroup, _editStatus, _editSection]
  pure NoContent

-- | Delete a category.
deleteCategory :: DB -> RequestDetails -> Uid Category -> Guider NoContent
deleteCategory db requestDetails catId = do
  _ <- getCategoryOrFail db catId
  eitherEdit <- dbUpdate db (DeleteCategory catId)
  case eitherEdit of
    Left _ -> pure NoContent
    Right _edit -> do
      config <- ask
      addEdit db config requestDetails _edit
      pure NoContent

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Create a new item, given the name.
--
-- Returns the ID of the created item. Unlike 'createCategory', allows items
-- with duplicated names.
createItem :: DB -> RequestDetails -> Uid Category -> Text -> Guider (Uid Item)
createItem db requestDetails catId name' = do
  _ <- getCategoryOrFail db catId
  when (T.null name') $ throwError err400{errBody = "Name not provided"}
  itemId <- randomShortUid
  -- If the item name looks like a Hackage library, assume it's a Hackage
  -- library.
  let isAllowedChar c = isAscii c && (isAlphaNum c || c == '-')
      looksLikeLibrary = T.all isAllowedChar name'
      kind' = if looksLikeLibrary then Library (Just name') else Other
  time <- liftIO getCurrentTime
  (_edit, _) <- dbUpdate db (AddItem catId itemId name' time kind')
  config <- ask
  addEdit db config requestDetails _edit
  pure itemId

-- TODO: move an item

-- | Set item's info
setItemInfo :: DB -> RequestDetails -> Uid Item -> CItemInfo -> Guider NoContent
setItemInfo db requestDetails itemId CItemInfo{..} = do
  _ <- getItemOrFail db itemId
  -- TODO diff and merge
  (_editName, _) <- dbUpdate db $ SetItemName itemId $ unH ciiName
  (_editGroup, _) <- dbUpdate db $ SetItemGroup itemId $ unH ciiGroup
  (_editLink, _) <- dbUpdate db $ SetItemLink itemId $ unH ciiLink
  (_editKind, _) <- dbUpdate db $ SetItemKind itemId $ unH ciiKind
  config <- ask
  mapM_ (addEdit db config requestDetails) [_editName, _editGroup, _editLink, _editKind]
  pure NoContent

-- | Set item's summary.
setItemSummary :: DB -> RequestDetails -> Uid Item -> CTextEdit -> Guider NoContent
setItemSummary db requestDetails itemId CTextEdit{..} = do
  serverModified <- markdownBlockMdSource . _itemDescription <$> getItemOrFail db itemId
  checkConflict CTextEdit{..} serverModified
  (_edit, _) <- dbUpdate db (SetItemDescription itemId $ unH cteModified)
  config <- ask
  addEdit db config requestDetails _edit
  pure NoContent

-- | Set item's ecosystem.
setItemEcosystem :: DB -> RequestDetails -> Uid Item -> CTextEdit -> Guider NoContent
setItemEcosystem db requestDetails itemId CTextEdit{..} = do
  serverModified <- markdownBlockMdSource . _itemEcosystem <$> getItemOrFail db itemId
  checkConflict CTextEdit{..} serverModified
  (_edit, _) <- dbUpdate db (SetItemEcosystem itemId $ unH cteModified)
  config <- ask
  addEdit db config requestDetails _edit
  pure NoContent

-- | Set item's notes.
setItemNotes :: DB -> RequestDetails -> Uid Item -> CTextEdit -> Guider NoContent
setItemNotes db requestDetails itemId CTextEdit{..} = do
  serverModified <- markdownTreeMdSource . _itemNotes <$> getItemOrFail db itemId
  checkConflict CTextEdit{..} serverModified
  (_edit, _) <- dbUpdate db (SetItemNotes itemId $ unH cteModified)
  config <- ask
  addEdit db config requestDetails _edit
  pure NoContent

-- | Delete an item.
deleteItem :: DB -> RequestDetails -> Uid Item -> Guider NoContent
deleteItem db requestDetails itemId = do
  eitherEdit <- dbUpdate db (DeleteItem itemId)
  case eitherEdit of
    Left _ -> pure NoContent
    Right _edit -> do
      config <- ask
      addEdit db config requestDetails _edit
      pure NoContent

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- TODO: move a trait

-- | Create a trait (pro/con).
createTrait :: DB -> RequestDetails -> Uid Item -> TraitType -> Text -> Guider (Uid Trait)
createTrait db requestDetails itemId traitType text = do
  when (T.null text) $ throwError err400{errBody = "Trait text not provided"}
  traitId <- randomShortUid
  (_edit, _) <- case traitType of
    Con -> dbUpdate db (AddCon itemId traitId text)
    Pro -> dbUpdate db (AddPro itemId traitId text)
  config <- ask
  addEdit db config requestDetails _edit
  pure traitId

-- | Update the text of a trait (pro/con).
setTrait :: DB -> RequestDetails -> Uid Item -> Uid Trait -> CTextEdit -> Guider NoContent
setTrait db requestDetails itemId traitId CTextEdit{..} = do
  serverModified <- markdownInlineMdSource . _traitContent <$> getTraitOrFail db itemId traitId
  checkConflict CTextEdit{..} serverModified
  (_edit, _) <- dbUpdate db (SetTraitContent itemId traitId $ unH cteModified)
  config <- ask
  addEdit db config requestDetails _edit
  pure NoContent

-- | Delete a trait (pro/con).
deleteTrait :: DB -> RequestDetails -> Uid Item -> Uid Trait -> Guider NoContent
deleteTrait db requestDetails itemId traitId = do
  eitherEdit <- dbUpdate db (DeleteTrait itemId traitId)
  case eitherEdit of
    Left _ -> pure NoContent
    Right _edit -> do
      config <- ask
      addEdit db config requestDetails _edit
      pure NoContent

----------------------------------------------------------------------------
-- Search
----------------------------------------------------------------------------

-- | Site-wide search.
--
-- Returns at most 100 results.
search :: DB -> Text -> Guider [CSearchResult]
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

-- Call this whenever any user-made change is applied to the database.
addEdit :: MonadIO m => DB -> Config -> RequestDetails -> Edit -> m ()
addEdit db Config{..} RequestDetails{..} edit = unless (isVacuousEdit edit) $ do
    time <- liftIO getCurrentTime
    dbUpdate db $ RegisterEdit edit ip time
    dbUpdate db $ RegisterAction (Action'Edit edit) ip time _baseUrl rdReferer rdUserAgent
  where
    ip :: Maybe IP
    ip = read . T.unpack <$> rdIp

-- | Helper. Get a category from database and throw error 404 when it doesn't exist.
getCategoryOrFail :: DB -> Uid Category -> Guider Category
getCategoryOrFail db catId = do
  dbQuery db (GetCategoryMaybe catId) >>= \case
    Nothing  -> throwError $ err404 {errBody = "Category not found"}
    Just cat -> pure cat

-- | Helper. Get an item from database and throw error 404 when the item doesn't exist.
getItemOrFail :: DB -> Uid Item -> Guider Item
getItemOrFail db itemId = do
  dbQuery db (GetItemMaybe itemId) >>= \case
    Nothing  -> throwError $ err404 {errBody = "Item not found"}
    Just item -> pure item

-- | Helper. Get a trait from database and throw error 404 when
-- either the item or the trait doesn't exist.
getTraitOrFail :: DB -> Uid Item -> Uid Trait -> Guider Trait
getTraitOrFail db itemId traitId = do
  dbQuery db (GetItemMaybe itemId) >>= \case
    Nothing  -> throwError $ err404 {errBody = "Item not found"}
    Just _ -> do
      dbQuery db (GetTraitMaybe itemId traitId) >>= \case
        Nothing -> throwError $ err404 {errBody = "Trait not found"}
        Just trait -> pure trait

-- | Checker. When states of database before and after editing is different, fail with a conflict data.
checkConflict :: CTextEdit -> Text -> Guider ()
checkConflict CTextEdit{..} serverModified = do
  let original = unH cteOriginal
  let modified = unH cteModified
  when (original /= serverModified) $ do
    let merged = merge original modified serverModified
    let conflict = CMergeConflict
          { cmcOriginal = cteOriginal
          , cmcModified = cteModified
          , cmcServerModified = H serverModified
          , cmcMerged = H merged
          }
    throwError $ err409 {errBody = encode conflict}
