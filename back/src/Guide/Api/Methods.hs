{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Guide.Api.Methods where


import Imports

import Control.Monad.Extra (whenJust)
import Data.Acid as Acid
import Data.Aeson (encode)
import Data.Text (Text)
import Servant

import Guide.Logger
import Guide.Api.Guider (Context (..), Guider)
import Guide.Matomo (Matomo (..), postMatomo)
import Guide.Api.Types
import Guide.Api.Utils
import Guide.Config (Config (..))
import Guide.Diff (merge)
import Guide.Markdown (MarkdownBlock (..), MarkdownInline (..), MarkdownTree (..))
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
getCategories :: Guider [CCategoryInfo]
getCategories = do
  debugT "getCategories"
  dbQuery GetCategories <&> map toCCategoryInfo

-- | Get a single category and all of its items.
getCategory :: Uid Category -> Guider CCategoryFull
getCategory catId = push "getCategory" $ attr "catId" (value catId) $ do
  debugT $ "handler called"
  toCCategoryFull <$> getCategoryOrFail catId

-- | Create a new category, given the title and the grandparent (aka group).
--
-- Returns the ID of the created category (or of the existing one if the
-- category with this title exists already).
createCategory :: Text -> Text -> Guider (Uid Category)
createCategory title' group' = do
  debugT $ "createCategory: title = \"" <> title' <> "\", group =\"" <> group' <> "\""
  when (T.null title') $ throwError err400{errReasonPhrase = "Title not provided"}
  when (T.null group') $ throwError err400{errReasonPhrase = "Group' not provided"}
  -- If the category exists already, don't create it
  cats <- view categories <$> dbQuery GetGlobalState
  let isDuplicate cat = T.toCaseFold (cat^.title) == T.toCaseFold title'
        && T.toCaseFold (cat^.group_) == T.toCaseFold group'
  case find isDuplicate cats of
    Just c  -> return (c^.uid)
    Nothing -> do
      catId <- randomShortUid
      time <- liftIO getCurrentTime
      addEdit . fst =<< dbUpdate (AddCategory catId title' group' time)
      return catId

-- | Edit categoty's note.
setCategoryNotes :: Uid Category -> CTextEdit -> Guider NoContent
setCategoryNotes catId CTextEdit{..} = do
  debugT $ "setCategoryNotes: " +|| catId ||+ ""
  serverModified <- markdownBlockMdSource . _categoryNotes <$> getCategoryOrFail catId
  checkConflict CTextEdit{..} serverModified
  addEdit . fst =<< dbUpdate (SetCategoryNotes catId $ unH cteModified)
  pure NoContent

-- | Edit category's info (title, group, status, sections (pro/con, ecosystem, note)).
setCategoryInfo :: Uid Category -> CCategoryInfoEdit -> Guider NoContent
setCategoryInfo catId CCategoryInfoEdit{..} = do
  debugT $ "setCategoryInfo: " +|| catId ||+ ""
  category <- getCategoryOrFail catId
  -- TODO diff and merge
  (editTitle, _) <- dbUpdate $ SetCategoryTitle catId $ unH ccieTitle
  (editGroup, _) <- dbUpdate $ SetCategoryGroup catId $ unH ccieGroup
  (editStatus, _) <- dbUpdate $ SetCategoryStatus catId ccieStatus
  let oldEnabledSections = category ^. enabledSections
  let newEnabledSections = unH ccieSections
  (editSection, _) <- dbUpdate $ ChangeCategoryEnabledSections catId
      (newEnabledSections S.\\ oldEnabledSections)
      (oldEnabledSections S.\\ newEnabledSections)
  mapM_ addEdit [editTitle, editGroup, editStatus, editSection]
  pure NoContent

-- | Delete a category.
deleteCategory :: Uid Category -> Guider NoContent
deleteCategory catId = do
  debugT $ "deleteCategory: " +|| catId ||+ ""
  _ <- getCategoryOrFail catId
  dbUpdate (DeleteCategory catId) >>= mapM_ addEdit
  pure NoContent

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get item by item id
getItem :: Uid Item -> Guider CItemFull
getItem itemId = do
  debugT $ "getItem: " +|| itemId ||+ ""
  toCItemFull <$> getItemOrFail itemId

-- | Create a new item, given the name.
--
-- Returns the ID of the created item. Unlike 'createCategory', allows items
-- with duplicated names.
createItem :: Uid Category -> Text -> Guider (Uid Item)
createItem catId name' = do
  debugT $ "createItem in category " +|| catId ||+ " with name" +| name' |+ ""
  _ <- getCategoryOrFail catId
  when (T.null name') $ throwError err400{errReasonPhrase = "Name not provided"}
  itemId <- randomShortUid
  time <- liftIO getCurrentTime
  addEdit . fst =<< dbUpdate (AddItem catId itemId name' time)
  pure itemId

-- TODO: move an item

-- | Modify item info. Fields that are not present ('Nothing') are not modified.
setItemInfo :: Uid Item -> CItemInfoEdit -> Guider NoContent
setItemInfo itemId CItemInfoEdit{..} = do
  debugT $ "setItemInfo: " +|| itemId ||+ ""
  _ <- getItemOrFail itemId
  -- TODO diff and merge
  whenJust (unH ciieName) $ \ciieName' ->
    addEdit . fst =<< dbUpdate (SetItemName itemId ciieName')
  whenJust (unH ciieGroup) $ \ciieGroup' ->
    addEdit . fst =<< dbUpdate (SetItemGroup itemId ciieGroup')
  whenJust (unH ciieHackage) $ \ciieHackage' ->
    addEdit . fst =<< dbUpdate (SetItemHackage itemId ciieHackage')
  whenJust (unH ciieLink) $ \ciieLink' -> do
    addEdit . fst =<< dbUpdate (SetItemLink itemId ciieLink')
  pure NoContent

-- | Set item's summary.
setItemSummary :: Uid Item -> CTextEdit -> Guider NoContent
setItemSummary itemId CTextEdit{..} = do
  debugT $ "setItemSummary: " +|| itemId ||+ ""
  serverModified <- markdownBlockMdSource . _itemSummary <$> getItemOrFail itemId
  checkConflict CTextEdit{..} serverModified
  addEdit . fst =<< dbUpdate (SetItemSummary itemId $ unH cteModified)
  pure NoContent

-- | Set item's ecosystem.
setItemEcosystem :: Uid Item -> CTextEdit -> Guider NoContent
setItemEcosystem itemId CTextEdit{..} = do
  debugT $ "setItemEcosystem: " +|| itemId ||+ ""
  serverModified <- markdownBlockMdSource . _itemEcosystem <$> getItemOrFail itemId
  checkConflict CTextEdit{..} serverModified
  addEdit . fst =<< dbUpdate (SetItemEcosystem itemId $ unH cteModified)
  pure NoContent

-- | Set item's notes.
setItemNotes :: Uid Item -> CTextEdit -> Guider NoContent
setItemNotes  itemId CTextEdit{..} = do
  debugT $ "setItemNotes: " +|| itemId ||+ ""
  serverModified <- markdownTreeMdSource . _itemNotes <$> getItemOrFail itemId
  checkConflict CTextEdit{..} serverModified
  addEdit . fst =<< dbUpdate (SetItemNotes itemId $ unH cteModified)
  pure NoContent

-- | Delete an item.
deleteItem :: Uid Item -> Guider NoContent
deleteItem itemId = do
  debugT $ "deleteItem: " +|| itemId ||+ ""
  _ <- getItemOrFail itemId
  dbUpdate (DeleteItem itemId) >>= mapM_ addEdit
  pure NoContent

-- | Move item up or down
moveItem :: Uid Item -> CMove -> Guider NoContent
moveItem itemId CMove{..} = do
  debugT $ "moveItem: " +|| itemId ||+ ""
  _ <- getItemOrFail itemId
  addEdit =<< dbUpdate (MoveItem itemId (cmDirection == DirectionUp))
  pure NoContent

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------
-- | Get a trait (pro/con)
getTrait :: Uid Item -> Uid Trait -> Guider CTrait
getTrait itemId traitId = do
  debugT $ "getTrait: " +|| itemId ||+ " " +|| traitId ||+ ""
  toCTrait <$> getTraitOrFail itemId traitId

-- | Create a trait (pro/con).
createTrait :: Uid Item -> CCreateTrait -> Guider (Uid Trait)
createTrait itemId CCreateTrait{..} = do
  debugT $ "createTrait: " +|| itemId ||+ ""
  when (T.null cctContent) $ throwError err400{errReasonPhrase = "Trait text not provided"}
  traitId <- randomShortUid
  addEdit . fst =<< case cctType of
    Con -> dbUpdate (AddCon itemId traitId cctContent)
    Pro -> dbUpdate (AddPro itemId traitId cctContent)
  pure traitId

-- | Update the text of a trait (pro/con).
setTrait :: Uid Item -> Uid Trait -> CTextEdit -> Guider NoContent
setTrait itemId traitId CTextEdit{..} = do
  debugT $ "setTrait: " +|| itemId ||+ " " +|| traitId ||+ ""
  serverModified <- markdownInlineMdSource . _traitContent <$> getTraitOrFail itemId traitId
  checkConflict CTextEdit{..} serverModified
  addEdit . fst =<< dbUpdate (SetTraitContent itemId traitId $ unH cteModified)
  pure NoContent

-- | Delete a trait (pro/con).
deleteTrait :: Uid Item -> Uid Trait -> Guider NoContent
deleteTrait itemId traitId = do
  debugT $ "deleteTrait: " +|| itemId ||+ " " +|| traitId ||+ ""
  _ <- getTraitOrFail itemId traitId
  dbUpdate (DeleteTrait itemId traitId) >>= mapM_ addEdit
  pure NoContent

-- | Move trait up or down
moveTrait :: Uid Item -> Uid Trait -> CMove -> Guider NoContent
moveTrait itemId traitId CMove{..} = do
  debugT $ "moveTrait: " +|| itemId ||+ " " +|| traitId ||+ ""
  _ <- getTraitOrFail itemId traitId
  addEdit =<< dbUpdate (MoveTrait itemId traitId (cmDirection == DirectionUp))
  pure NoContent

----------------------------------------------------------------------------
-- Search
----------------------------------------------------------------------------

-- | Site-wide search.
--
-- Returns at most 100 results.
search :: Text -> Guider [CSearchResult]
search searchQuery = do
  debugT $ "search: " +|| searchQuery ||+ " "
  gs <- dbQuery GetGlobalState
  pure $ map toCSearchResult $ take 100 $ Search.search searchQuery gs

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Update something in the database.
dbUpdate :: (EventState event ~ GlobalState, UpdateEvent event, Show event)
         => event -> Guider (EventResult event)
dbUpdate x = do
  debugT $ "dbUpdate: " +|| x ||+ ""
  Context{..} <- ask
  liftIO $ do
    Acid.update cDB SetDirty
    Acid.update cDB x

-- | Read something from the database.
dbQuery :: (EventState event ~ GlobalState, QueryEvent event, Show event)
        => event -> Guider (EventResult event)
dbQuery x = do
  debugT $ "dbQuery: " +|| x ||+ ""
  Context{..} <- ask
  liftIO $ Acid.query cDB x

-- Call this whenever any user-made change is applied to the database.
addEdit :: Edit -> Guider ()
addEdit edit = do
  debugT $ "addEdit: " +|| edit ||+ ""
  unless (isVacuousEdit edit) $ do
    debugT $ "addEdit: it makes sense to edit."
    time <- liftIO getCurrentTime
    Context Config{..} _ RequestDetails{..} <- ask
    dbUpdate $ RegisterEdit edit rdIp time
    dbUpdate $ RegisterAction (Action'Edit edit) rdIp time _baseUrl rdReferer rdUserAgent
    postMatomo $ Matomo rdIp rdUserAgent rdReferer edit

-- | Helper. Get a category from database and throw error 404 when it doesn't exist.
getCategoryOrFail :: Uid Category -> Guider Category
getCategoryOrFail catId = do
  debugT $ "getCategoryOrFail: " +|| catId ||+ ""
  dbQuery (GetCategoryMaybe catId) >>= \case
    Nothing -> throwError $ err404 {errReasonPhrase = "Category not found"}
    Just cat -> pure cat

-- | Helper. Get an item from database and throw error 404 when the item doesn't exist.
getItemOrFail :: Uid Item -> Guider Item
getItemOrFail itemId = do
  debugT $ "getItemOrFail: " +|| itemId ||+ ""
  dbQuery (GetItemMaybe itemId) >>= \case
    Nothing -> throwError $ err404 {errReasonPhrase = "Item not found"}
    Just item -> pure item

-- | Helper. Get a trait from database and throw error 404 when
-- either the item or the trait doesn't exist.
getTraitOrFail :: Uid Item -> Uid Trait -> Guider Trait
getTraitOrFail itemId traitId = do
  debugT $ "getTraitOrFail: " +|| itemId ||+ ", " +|| traitId ||+ ""
  dbQuery (GetItemMaybe itemId) >>= \case
    Nothing -> throwError $ err404 {errReasonPhrase = "Item not found"}
    Just _ -> do
      dbQuery (GetTraitMaybe itemId traitId) >>= \case
        Nothing -> throwError $ err404 {errReasonPhrase = "Trait not found"}
        Just trait -> pure trait

-- | Checker. When states of database before and after editing is different, fail with a conflict data.
checkConflict :: CTextEdit -> Text -> Guider ()
checkConflict CTextEdit{..} serverModified = do
  debugT $ "checkConflict"
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
    throwError $ err409 {
      errReasonPhrase = "Merge conflict occurred",
      errBody = encode conflict
    }
