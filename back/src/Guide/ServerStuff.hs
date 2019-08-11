{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


-- | Spock state, functions for manipulating it, handler helpers, and so on.
--
-- TODO: this is not the best name for a module. Really.
module Guide.ServerStuff
(
  ServerState(..),
  getConfig,
  dbUpdate,
  dbQuery,

  -- * Edits
  addEdit,
  undoEdit,

  -- * Handler helpers
  itemVar,
  categoryVar,
  traitVar,
)
where


import Imports

-- Web
import Web.Routing.Combinators (PathState (..))
import Web.Spock hiding (get, head, text)
-- acid-state
import Data.Acid as Acid

import Guide.Config
import Guide.Markdown
import Guide.State
import Guide.Types
import Guide.Utils

import qualified Web.Spock as Spock


-- | Global state of the site.
data ServerState = ServerState {
  _config :: Config,                -- ^ Config (doesn't change in runtime)
  _db     :: DB                     -- ^ DB connection
  }

-- | Get config in a Spock monad.
getConfig :: (Monad m, HasSpock m, SpockState m ~ ServerState)
          => m Config
getConfig = _config <$> Spock.getState

-- | Update something in the database.
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
-- Edits
----------------------------------------------------------------------------

-- | Remember an edit.
--
-- Call this whenever any user-made change is applied to the database.
addEdit :: (MonadIO m, HasSpock (ActionCtxT ctx m),
            SpockState (ActionCtxT ctx m) ~ ServerState)
        => Edit -> ActionCtxT ctx m ()
addEdit ed = do
  (time, mbIP, _mbReferrer, _mbUA) <- getRequestDetails
  unless (isVacuousEdit ed) $ do
    dbUpdate (RegisterEdit ed mbIP time)

-- | Do an action that would undo an edit. 'Left' signifies failure.
--
-- TODO: many of these don't work when the changed category, item, etc has
-- been deleted; this should change.
undoEdit :: (MonadIO m, HasSpock m, SpockState m ~ ServerState)
         => Edit -> m (Either String ())
undoEdit (Edit'AddCategory catId _ _) = do
  void <$> dbUpdate (DeleteCategory catId)
undoEdit (Edit'AddItem _catId itemId _) = do
  void <$> dbUpdate (DeleteItem itemId)
undoEdit (Edit'AddPro itemId traitId _) = do
  void <$> dbUpdate (DeleteTrait itemId traitId)
undoEdit (Edit'AddCon itemId traitId _) = do
  void <$> dbUpdate (DeleteTrait itemId traitId)
undoEdit (Edit'SetCategoryTitle catId old new) = do
  now <- categoryTitle <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "title has been changed further")
    else Right () <$ dbUpdate (SetCategoryTitle catId old)
undoEdit (Edit'SetCategoryGroup catId old new) = do
  now <- categoryGroup <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "group has been changed further")
    else Right () <$ dbUpdate (SetCategoryGroup catId old)
undoEdit (Edit'SetCategoryStatus catId old new) = do
  now <- categoryStatus <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "status has been changed further")
    else Right () <$ dbUpdate (SetCategoryStatus catId old)
undoEdit (Edit'ChangeCategoryEnabledSections catId toEnable toDisable) = do
  enabledNow <- categoryEnabledSections <$> dbQuery (GetCategory catId)
  if any (`elem` enabledNow) toDisable || any (`notElem` enabledNow) toEnable
    then return (Left "enabled-sections has been changed further")
    else Right () <$ dbUpdate (ChangeCategoryEnabledSections catId toDisable toEnable)
undoEdit (Edit'SetCategoryNotes catId old new) = do
  now <- markdownBlockSource . categoryNotes <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "notes have been changed further")
    else Right () <$ dbUpdate (SetCategoryNotes catId old)
undoEdit (Edit'SetItemName itemId old new) = do
  now <- itemName <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "name has been changed further")
    else Right () <$ dbUpdate (SetItemName itemId old)
undoEdit (Edit'SetItemLink itemId old new) = do
  now <- itemLink <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "link has been changed further")
    else Right () <$ dbUpdate (SetItemLink itemId old)
undoEdit (Edit'SetItemGroup _ _ _) = do
  return (Left "groups are not supported anymore")
undoEdit (Edit'SetItemHackage itemId old new) = do
  now <- itemHackage <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "Hackage name has been changed further")
    else Right () <$ dbUpdate (SetItemHackage itemId old)
undoEdit (Edit'SetItemSummary itemId old new) = do
  now <- markdownBlockSource . itemSummary <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "description has been changed further")
    else Right () <$ dbUpdate (SetItemSummary itemId old)
undoEdit (Edit'SetItemNotes itemId old new) = do
  now <- markdownTreeSource . itemNotes <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "notes have been changed further")
    else Right () <$ dbUpdate (SetItemNotes itemId old)
undoEdit (Edit'SetItemEcosystem itemId old new) = do
  now <- markdownBlockSource . itemEcosystem <$> dbQuery (GetItem itemId)
  if now /= new
    then return (Left "ecosystem has been changed further")
    else Right () <$ dbUpdate (SetItemEcosystem itemId old)
undoEdit (Edit'SetTraitContent itemId traitId old new) = do
  now <- markdownInlineSource . traitContent <$> dbQuery (GetTrait itemId traitId)
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
