{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

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
getCategories =
  logHandler "getCategories" [] $ do
    dbQuery GetCategories <&> map toCCategoryInfo

-- | Get a single category and all of its items.
getCategory :: Uid Category -> Guider CCategoryFull
getCategory catId =
  logHandler "getCategory" [attr "catId" catId] $ do
    toCCategoryFull <$> getCategoryOrFail catId

-- | Create a new category, given the title and the grandparent (aka group).
--
-- Returns the ID of the created category (or of the existing one if the
-- category with this title exists already).
createCategory :: Text -> Text -> Guider (Uid Category)
createCategory title' group' =
  logHandler "createCategory" [attr "title" title', attr "group" group'] $ do
    when (T.null title') $ throwError err400{errReasonPhrase = "Title not provided"}
    when (T.null group') $ throwError err400{errReasonPhrase = "Group not provided"}
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
setCategoryNotes catId CTextEdit{..} =
  logHandler "setCategoryNotes" [attr "catId" catId] $ do
    serverModified <- markdownBlockMdSource . _categoryNotes <$> getCategoryOrFail catId
    checkConflict CTextEdit{..} serverModified
    addEdit . fst =<< dbUpdate (SetCategoryNotes catId $ unH cteModified)
    pure NoContent

-- | Edit category's info (title, group, status, sections (pro/con, ecosystem, note)).
setCategoryInfo :: Uid Category -> CCategoryInfoEdit -> Guider NoContent
setCategoryInfo catId CCategoryInfoEdit{..} =
  logHandler "setCategoryInfo" [attr "catId" catId] $ do
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
deleteCategory catId =
  logHandler "deleteCategory" [attr "catId" catId] $ do
    _ <- getCategoryOrFail catId
    dbUpdate (DeleteCategory catId) >>= mapM_ addEdit
    pure NoContent

----------------------------------------------------------------------------
-- Items
----------------------------------------------------------------------------

-- | Get item by item id
getItem :: Uid Item -> Guider CItemFull
getItem itemId =
  logHandler "getItem" [attr "itemId" itemId] $ do
    toCItemFull <$> getItemOrFail itemId

-- | Create a new item, given the name.
--
-- Returns the ID of the created item. Unlike 'createCategory', allows items
-- with duplicated names.
createItem :: Uid Category -> Text -> Guider (Uid Item)
createItem catId name' =
  logHandler "createItem" [attr "catId" catId, attr "name" name'] $ do
    _ <- getCategoryOrFail catId
    when (T.null name') $ throwError err400{errReasonPhrase = "Name not provided"}
    itemId <- randomShortUid
    time <- liftIO getCurrentTime
    addEdit . fst =<< dbUpdate (AddItem catId itemId name' time)
    pure itemId

-- | Modify item info. Fields that are not present ('Nothing') are not modified.
setItemInfo :: Uid Item -> CItemInfoEdit -> Guider NoContent
setItemInfo itemId CItemInfoEdit{..} =
  logHandler "setItemInfo" [attr "itemId" itemId] $ do
    void $ getItemOrFail itemId
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
setItemSummary itemId CTextEdit{..} =
  logHandler "setItemSummary" [attr "itemId" itemId] $ do
    serverModified <- markdownBlockMdSource . _itemSummary <$> getItemOrFail itemId
    checkConflict CTextEdit{..} serverModified
    addEdit . fst =<< dbUpdate (SetItemSummary itemId $ unH cteModified)
    pure NoContent

-- | Set item's ecosystem.
setItemEcosystem :: Uid Item -> CTextEdit -> Guider NoContent
setItemEcosystem itemId CTextEdit{..} =
  logHandler "setItemEcosystem" [attr "itemId" itemId] $ do
    serverModified <- markdownBlockMdSource . _itemEcosystem <$> getItemOrFail itemId
    checkConflict CTextEdit{..} serverModified
    addEdit . fst =<< dbUpdate (SetItemEcosystem itemId $ unH cteModified)
    pure NoContent

-- | Set item's notes.
setItemNotes :: Uid Item -> CTextEdit -> Guider NoContent
setItemNotes itemId CTextEdit{..} =
  logHandler "setItemNotes" [attr "itemId" itemId] $ do
    serverModified <- markdownTreeMdSource . _itemNotes <$> getItemOrFail itemId
    checkConflict CTextEdit{..} serverModified
    addEdit . fst =<< dbUpdate (SetItemNotes itemId $ unH cteModified)
    pure NoContent

-- | Delete an item.
deleteItem :: Uid Item -> Guider NoContent
deleteItem itemId =
  logHandler "deleteItem" [attr "itemId" itemId] $ do
    void $ getItemOrFail itemId
    dbUpdate (DeleteItem itemId) >>= mapM_ addEdit
    pure NoContent

-- | Move item up or down
moveItem :: Uid Item -> CMove -> Guider NoContent
moveItem itemId CMove{..} =
  logHandler "moveItem" [attr "itemId" itemId] $ do
    void $ getItemOrFail itemId
    addEdit =<< dbUpdate (MoveItem itemId (cmDirection == DirectionUp))
    pure NoContent

----------------------------------------------------------------------------
-- Traits
----------------------------------------------------------------------------

-- | Get a trait (pro/con)
getTrait :: Uid Item -> Uid Trait -> Guider CTrait
getTrait itemId traitId =
  logHandler "getTrait" [attr "itemId" itemId, attr "traitId" traitId] $ do
    toCTrait <$> getTraitOrFail itemId traitId

-- | Create a trait (pro/con).
createTrait :: Uid Item -> CCreateTrait -> Guider (Uid Trait)
createTrait itemId CCreateTrait{..} =
  logHandler "createTrait" [attr "itemId" itemId] $ do
    when (T.null cctContent) $ throwError err400{errReasonPhrase = "Trait text not provided"}
    traitId <- randomShortUid
    addEdit . fst =<< case cctType of
      Con -> dbUpdate (AddCon itemId traitId cctContent)
      Pro -> dbUpdate (AddPro itemId traitId cctContent)
    pure traitId

-- | Update the text of a trait (pro/con).
setTrait :: Uid Item -> Uid Trait -> CTextEdit -> Guider NoContent
setTrait itemId traitId CTextEdit{..} =
  logHandler "setTrait" [attr "itemId" itemId, attr "traitId" traitId] $ do
    serverModified <- markdownInlineMdSource . _traitContent <$> getTraitOrFail itemId traitId
    checkConflict CTextEdit{..} serverModified
    addEdit . fst =<< dbUpdate (SetTraitContent itemId traitId $ unH cteModified)
    pure NoContent

-- | Delete a trait (pro/con).
deleteTrait :: Uid Item -> Uid Trait -> Guider NoContent
deleteTrait itemId traitId =
  logHandler "deleteTrait" [attr "itemId" itemId, attr "traitId" traitId] $ do
    void $ getTraitOrFail itemId traitId
    dbUpdate (DeleteTrait itemId traitId) >>= mapM_ addEdit
    pure NoContent

-- | Move trait up or down
moveTrait :: Uid Item -> Uid Trait -> CMove -> Guider NoContent
moveTrait itemId traitId CMove{..} =
  logHandler "moveTrait" [attr "itemId" itemId, attr "traitId" traitId] $ do
    void $ getTraitOrFail itemId traitId
    addEdit =<< dbUpdate (MoveTrait itemId traitId (cmDirection == DirectionUp))
    pure NoContent

----------------------------------------------------------------------------
-- Search
----------------------------------------------------------------------------

-- | Site-wide search.
--
-- Returns at most 100 results.
search :: Text -> Guider [CSearchResult]
search searchQuery =
  logHandler "search" [attr "searchQuery" searchQuery] $ do
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

-- | Call this whenever any user-made change is applied to the database.
addEdit :: Edit -> Guider ()
addEdit edit = push "addEdit" $ attr "edit" edit $ do
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
  dbQuery (GetCategoryMaybe catId) >>= \case
    Nothing -> throwError $ err404 {errReasonPhrase = "Category not found"}
    Just cat -> pure cat

-- | Helper. Get an item from database and throw error 404 when the item doesn't exist.
getItemOrFail :: Uid Item -> Guider Item
getItemOrFail itemId = do
  dbQuery (GetItemMaybe itemId) >>= \case
    Nothing -> throwError $ err404 {errReasonPhrase = "Item not found"}
    Just item -> pure item

-- | Helper. Get a trait from database and throw error 404 when
-- either the item or the trait doesn't exist.
getTraitOrFail :: Uid Item -> Uid Trait -> Guider Trait
getTraitOrFail itemId traitId = do
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

-- | Log invocation of a handler.
--
-- This function exists because otherwise it's easy to log that the handler
-- has been called (at the very beginning of the handler).
--
-- TODO: it would be nice to somehow automatically get this logging from
-- Servant for all handlers.
logHandler
  :: Text                    -- ^ Handler name
  -> [Guider a -> Guider a]  -- ^ Handler arguments, usually created by @attr "key" value@
  -> Guider a
  -> Guider a
logHandler hName args body =
    foldr ($) (debugT "Handler called" >> body) (push hName : args)
