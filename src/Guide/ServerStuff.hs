{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


{- |
Spock state, functions for manipulating it, handler helpers, and so on.

TODO: this is not the best name for a module. Really.
-}
module Guide.ServerStuff
(
  ServerState(..),
    getConfig,
    getManager,
  dbUpdate,
  dbQuery,

  -- * Cache
  uncache,
  invalidateCache',

  -- * Edits
  addEdit,
  undoEdit,
  invalidateCacheForEdit,

  -- * Handler helpers
  itemVar,
  categoryVar,
  traitVar,
)
where


import Imports

-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Routing.Combinators (PathState(..))
-- http-client
import Network.HTTP.Client (Manager)
-- acid-state
import Data.Acid as Acid

import Guide.Config
import Guide.State
import Guide.Types
import Guide.Cache
import Guide.Utils
import Guide.Markdown


-- | Global state of the site.
data ServerState = ServerState {
  _config      :: Config,                -- ^ Config (doesn't change in runtime)
  _db          :: DB,                    -- ^ DB connection
  _httpManager :: Manager                -- ^ HTTP.Client.Manager
  }

-- | Get config in a Spock monad.
getConfig :: (Monad m, HasSpock m, SpockState m ~ ServerState)
          => m Config
getConfig = _config <$> Spock.getState

-- | Get http manager in a Spock monad.
getManager :: (Monad m, HasSpock m, SpockState m ~ ServerState)
           => m Manager
getManager = _httpManager <$> Spock.getState

-- | Update something in the database. Don't forget to 'invalidateCache' or
-- use 'uncache' when you update something that is cached.
--
-- Example: @dbUpdate (DeleteCategory catId)@
--
dbUpdate :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
             EventState event ~ GlobalState, UpdateEvent event)
         => event -> m (EventResult event)
dbUpdate x = do
  db <- _db <$> Spock.getState
  liftIO $ do
    Acid.update db SetDirty
    Acid.update db x

-- | Read something from the database.
--
-- Example: @dbQuery (GetCategory catId)@.
--
dbQuery :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
            EventState event ~ GlobalState, QueryEvent event)
        => event -> m (EventResult event)
dbQuery x = do
  db <- _db <$> Spock.getState
  liftIO $ Acid.query db x

----------------------------------------------------------------------------
-- Cache
----------------------------------------------------------------------------

-- | Do a database-modifying action and invalidate the cache /afterwards/.
--
-- To invalidate the cache properly, we use global state – for instance, if
-- item X belongs to category Y, and the item is deleted, we have to
-- invalidate the cache for the category (and for that we need to look at the
-- global state and find out which category has item X). However, we have to
-- use the state from /before/ the item is deleted – otherwise we'll think
-- that no category had the item.
--
-- Note: if you delete or modify something, wrap it into 'uncache'. However,
-- if you /add/ something, use 'invalidateCache'':
--
-- >>> addItem
-- >>> invalidateCache' (CacheItem ...)
uncache
  :: (MonadIO m, HasSpock (ActionCtxT ctx m),
      SpockState (ActionCtxT ctx m) ~ ServerState)
  => CacheKey -> ActionCtxT ctx m a -> ActionCtxT ctx m a
uncache key act = do
  gs <- dbQuery GetGlobalState
  x <- act
  invalidateCache gs key
  return x

invalidateCache'
  :: (MonadIO m, HasSpock (ActionCtxT ctx m),
      SpockState (ActionCtxT ctx m) ~ ServerState)
  => CacheKey -> ActionCtxT ctx m ()
invalidateCache' key = do
  gs <- dbQuery GetGlobalState
  invalidateCache gs key

----------------------------------------------------------------------------
-- Edits
----------------------------------------------------------------------------

-- | Remember an edit.
--
-- Call this whenever any user-made change is applied to the database.
addEdit :: (MonadIO m, HasSpock (ActionCtxT ctx m),
            SpockState (ActionCtxT ctx m) ~ ServerState)
        => Edit -> ActionCtxT ctx m ()
addEdit ed = do
  (time, mbIP, mbReferrer, mbUA) <- getRequestDetails
  unless (isVacuousEdit ed) $ do
    dbUpdate (RegisterEdit ed mbIP time)
    baseUrl <- _baseUrl <$> getConfig
    dbUpdate (RegisterAction (Action'Edit ed)
                mbIP time baseUrl mbReferrer mbUA)

-- | Do an action that would undo an edit.
--
-- 'Left' signifies failure.
--
-- This doesn't do cache invalidation (you have to do it at the call site
-- using 'invalidateCacheForEdit').
--
-- TODO: make this do cache invalidation.
--
-- TODO: many of these don't work when the changed category, item, etc has
-- been deleted; this should change.
undoEdit :: (MonadIO m, HasSpock m, SpockState m ~ ServerState)
         => Edit -> m (Either String ())
undoEdit (Edit'AddCategory catId _) = do
  void <$> dbUpdate (DeleteCategory catId)
undoEdit (Edit'AddItem _catId itemId _) = do
  void <$> dbUpdate (DeleteItem itemId)
undoEdit (Edit'AddPro itemId traitId _) = do
  void <$> dbUpdate (DeleteTrait itemId traitId)
undoEdit (Edit'AddCon itemId traitId _) = do
  void <$> dbUpdate (DeleteTrait itemId traitId)
undoEdit (Edit'SetCategoryTitle catId old new) = do
  now <- view title <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "title has been changed further")
    else Right () <$ dbUpdate (SetCategoryTitle catId old)
undoEdit (Edit'SetCategoryGroup catId old new) = do
  now <- view group_ <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "group has been changed further")
    else Right () <$ dbUpdate (SetCategoryGroup catId old)
undoEdit (Edit'SetCategoryStatus catId old new) = do
  now <- view status <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "status has been changed further")
    else Right () <$ dbUpdate (SetCategoryStatus catId old)
undoEdit (Edit'ChangeCategoryEnabledSections catId toEnable toDisable) = do
  enabledNow <- view enabledSections <$> dbQuery (GetCategory catId)
  if any (`elem` enabledNow) toDisable || any (`notElem` enabledNow) toEnable
    then return (Left "enabled-sections has been changed further")
    else Right () <$ dbUpdate (ChangeCategoryEnabledSections catId toDisable toEnable)
undoEdit (Edit'SetCategoryNotes catId old new) = do
  now <- view (notes.mdText) <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "notes have been changed further")
    else Right () <$ dbUpdate (SetCategoryNotes catId old)
undoEdit (Edit'SetItemName itemId old new) = do
  now <- view name <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "name has been changed further")
    else Right () <$ dbUpdate (SetItemName itemId old)
undoEdit (Edit'SetItemLink itemId old new) = do
  now <- view link <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "link has been changed further")
    else Right () <$ dbUpdate (SetItemLink itemId old)
undoEdit (Edit'SetItemGroup itemId old new) = do
  now <- view group_ <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "group has been changed further")
    else Right () <$ dbUpdate (SetItemGroup itemId old)
undoEdit (Edit'SetItemKind itemId old new) = do
  now <- view kind <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "kind has been changed further")
    else Right () <$ dbUpdate (SetItemKind itemId old)
undoEdit (Edit'SetItemDescription itemId old new) = do
  now <- view (description.mdText) <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "description has been changed further")
    else Right () <$ dbUpdate (SetItemDescription itemId old)
undoEdit (Edit'SetItemNotes itemId old new) = do
  now <- view (notes.mdText) <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "notes have been changed further")
    else Right () <$ dbUpdate (SetItemNotes itemId old)
undoEdit (Edit'SetItemEcosystem itemId old new) = do
  now <- view (ecosystem.mdText) <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "ecosystem has been changed further")
    else Right () <$ dbUpdate (SetItemEcosystem itemId old)
undoEdit (Edit'SetTraitContent itemId traitId old new) = do
  now <- view (content.mdText) <$> dbQuery (GetTrait itemId traitId)
  if now /= new
    then return (Left "trait has been changed further")
    else Right () <$ dbUpdate (SetTraitContent itemId traitId old)
undoEdit (Edit'DeleteCategory catId pos) = do
  dbUpdate (RestoreCategory catId pos)
undoEdit (Edit'DeleteItem itemId pos) = do
  dbUpdate (RestoreItem itemId pos)
undoEdit (Edit'DeleteTrait itemId traitId pos) = do
  dbUpdate (RestoreTrait itemId traitId pos)
undoEdit (Edit'MoveItem itemId direction) = do
  Right () <$ dbUpdate (MoveItem itemId (not direction))
undoEdit (Edit'MoveTrait itemId traitId direction) = do
  Right () <$ dbUpdate (MoveTrait itemId traitId (not direction))

-- | Given an edit, invalidate cache items that should be invalidated when
-- that edit is undone.
invalidateCacheForEdit
  :: (MonadIO m, HasSpock m, SpockState m ~ ServerState)
  => Edit -> m ()
invalidateCacheForEdit ed = do
  gs <- dbQuery GetGlobalState
  mapM_ (invalidateCache gs) $ case ed of
    Edit'AddCategory catId _ ->
        [CacheCategory catId]
    -- Normally invalidateCache should invalidate item's category
    -- automatically, but in this case it's *maybe* possible that the item
    -- has already been moved somewhere else and so we invalidate both just
    -- in case.
    Edit'AddItem catId itemId _ ->
        [CacheCategory catId, CacheItem itemId]
    Edit'AddPro itemId _ _ ->
        [CacheItemTraits itemId]
    Edit'AddCon itemId _ _ ->
        [CacheItemTraits itemId]
    Edit'SetCategoryTitle catId _ _ ->
        [CacheCategoryInfo catId]
    Edit'SetCategoryGroup catId _ _ ->
        [CacheCategoryInfo catId]
    Edit'SetCategoryStatus catId _ _ ->
        [CacheCategoryInfo catId]
    Edit'ChangeCategoryEnabledSections catId _ _ ->
        [CacheCategoryInfo catId]
    Edit'SetCategoryNotes catId _ _ ->
        [CacheCategoryNotes catId]
    Edit'SetItemName itemId _ _ ->
        [CacheItemInfo itemId]
    Edit'SetItemLink itemId _ _ ->
        [CacheItemInfo itemId]
    Edit'SetItemGroup itemId _ _ ->
        [CacheItemInfo itemId]
    Edit'SetItemKind itemId _ _ ->
        [CacheItemInfo itemId]
    Edit'SetItemDescription itemId _ _ ->
        [CacheItemDescription itemId]
    Edit'SetItemNotes itemId _ _ ->
        [CacheItemNotes itemId]
    Edit'SetItemEcosystem itemId _ _ ->
        [CacheItemEcosystem itemId]
    Edit'SetTraitContent itemId _ _ _ ->
        [CacheItemTraits itemId]
    Edit'DeleteCategory catId _ ->
        [CacheCategory catId]
    Edit'DeleteItem itemId _ ->
        [CacheItem itemId]
    Edit'DeleteTrait itemId _ _ ->
        [CacheItemTraits itemId]
    Edit'MoveItem itemId _ ->
        [CacheItem itemId]
    Edit'MoveTrait itemId _ _ ->
        [CacheItemTraits itemId]

----------------------------------------------------------------------------
-- Handler helpers
----------------------------------------------------------------------------

-- | A path piece for items
itemVar :: Path '[Uid Item] 'Open
itemVar = "item" <//> var

-- | A path piece for categories
categoryVar :: Path '[Uid Category] 'Open
categoryVar = "category" <//> var

-- | A path pieces for traits
traitVar :: Path '[Uid Trait] 'Open
traitVar = "trait" <//> var
