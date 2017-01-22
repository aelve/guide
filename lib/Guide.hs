{-# LANGUAGE
OverloadedStrings,
ScopedTypeVariables,
TypeFamilies,
DataKinds,
FlexibleContexts,
NoImplicitPrelude
  #-}


module Guide
(
  main,
  mainWith,
)
where


import BasePrelude hiding (Category)
-- Monads and monad transformers
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Morph
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Containers
import qualified Data.Map as M
-- Text
import Data.Text.All (Text)
import qualified Data.Text.All as T
import qualified Data.Text.Lazy.All as TL
-- Paths
import System.FilePath ((</>))
-- Network
import Data.IP (IP)
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Config
import Web.Routing.Combinators (PathState(..))
import Web.Spock.Lucid
import Lucid hiding (for_)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
-- Feeds
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Util  as Feed
import qualified Text.Atom.Feed  as Atom
-- Highlighting
import CMark.Highlight (styleToCss, pygments)
-- Monitoring
import qualified System.Remote.Monitoring as EKG
import qualified Network.Wai.Metrics      as EKG
import qualified System.Metrics.Gauge     as EKG.Gauge
-- acid-state
import Data.Acid as Acid
-- Time
import Data.Time
-- Deepseq
import Control.DeepSeq
-- IO
import System.IO
import qualified SlaveThread as Slave
-- Watching the templates directory
import qualified System.FSNotify as FSNotify

-- Local
import Config
import Types
import View
import JS (JS(..), allJSFunctions)
import Utils
import Markdown
import Cache
import Merge


{- Note [acid-state]
~~~~~~~~~~~~~~~~~~~~

This application doesn't use a database – instead, it uses
acid-state. Acid-state works as follows:

  * Everything is stored as Haskell values (in particular, all data is stored in 'GlobalState').

  * All changes to the state (and all queries) have to be done by using
    'dbUpdate'/'dbQuery' and types (GetItem, SetItemName, etc) from the
    Types.hs module.

  * When doing a 'dbUpdate', don't forget to 'invalidateCache'!

  * The data is kept in-memory, but all changes are logged to the disk (which
    lets us recover the state in case of a crash by reapplying the changes)
    and you can't access the state directly. When the application exits, it
    creates a snapshot of the state (called “checkpoint”) and writes it to
    the disk. Additionally, a checkpoint is created every hour (grep for
    “createCheckpoint”).

  * acid-state has a nasty feature – when the state hasn't changed,
    'createCheckpoint' appends it to the previous checkpoint. When state
    doesn't change for a long time, it means that checkpoints can grow to 100
    MB or more. So, we employ a dirty bit and use createCheckpoint' instead
    of createCheckpoint. The former only creates the checkpoint if the dirty
    bit is set, which is good.

  * When any type is changed, we have to write a migration function that
    would read the old version of the type and turn it into the new
    version. This is done by 'changelog' – you only need to provide the list
    of differences between the old type and the new type.

  * There are actually ways to access the state directly (GetGlobalState and
    SetGlobalState), but the latter should only be used when doing something
    one-off (e.g. if you need to migrate all IDs to a different ID scheme).

-}

-- | A pointer to an open acid-state database (allows making queries/updates,
-- creating checkpoints, etc).
type DB = AcidState GlobalState

-- | Update something in the database. Don't forget to 'invalidateCache' when
-- you update something that is cached.
dbUpdate :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
             EventState event ~ GlobalState, UpdateEvent event)
         => event -> m (EventResult event)
dbUpdate x = do
  db <- _db <$> Spock.getState
  liftIO $ do
    Acid.update db SetDirty
    Acid.update db x

-- | Read something from the database.
dbQuery :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
            EventState event ~ GlobalState, QueryEvent event)
        => event -> m (EventResult event)
dbQuery x = do
  db <- _db <$> Spock.getState
  liftIO $ Acid.query db x

------------------------------------------------------------------------------
-- Server state
------------------------------------------------------------------------------

data ServerState = ServerState {
  _config :: Config,
  _db     :: DB }

getConfig :: (Monad m, HasSpock m, SpockState m ~ ServerState)
          => m Config
getConfig = _config <$> Spock.getState

itemVar :: Path '[Uid Item] 'Open
itemVar = "item" <//> var

categoryVar :: Path '[Uid Category] 'Open
categoryVar = "category" <//> var

traitVar :: Path '[Uid Trait] 'Open
traitVar = "trait" <//> var

invalidateCache'
  :: (MonadIO m, HasSpock (ActionCtxT ctx m),
      SpockState (ActionCtxT ctx m) ~ ServerState)
  => CacheKey -> ActionCtxT ctx m ()
invalidateCache' key = do
  gs <- dbQuery GetGlobalState
  invalidateCache gs key

getDetails
  :: (MonadIO m, HasSpock (ActionCtxT ctx m))
  => ActionCtxT ctx m (UTCTime, Maybe IP, Maybe Text, Maybe Text)
getDetails = do
  time <- liftIO $ getCurrentTime
  mbForwardedFor <- liftA2 (<|>) (Spock.header "Forwarded-For")
                                 (Spock.header "X-Forwarded-For")
  mbIP <- case mbForwardedFor of
    Nothing -> sockAddrToIP . Wai.remoteHost <$> Spock.request
    Just ff -> case readMaybe (T.unpack ip) of
      Nothing -> error ("couldn't read Forwarded-For address: " ++
                        show ip ++ " (full header: " ++
                        show ff ++ ")")
      Just i  -> return (Just i)
      where
        addr = T.strip . snd . T.breakOnEnd "," $ ff
        ip -- [IPv6]:port
           | T.take 1 addr == "[" =
               T.drop 1 (T.takeWhile (/= ']') addr)
           -- IPv4 or IPv4:port
           | T.any (== '.') addr =
               T.takeWhile (/= ':') addr
           -- IPv6 without port
           | otherwise =
               addr
  mbReferrer <- Spock.header "Referer"
  mbUA       <- Spock.header "User-Agent"
  return (time, mbIP, mbReferrer, mbUA)

-- | Call this whenever a user edits the database.
addEdit :: (MonadIO m, HasSpock (ActionCtxT ctx m),
            SpockState (ActionCtxT ctx m) ~ ServerState)
        => Edit -> ActionCtxT ctx m ()
addEdit ed = do
  (time, mbIP, mbReferrer, mbUA) <- getDetails
  unless (isVacuousEdit ed) $ do
    dbUpdate (RegisterEdit ed mbIP time)
    baseUrl <- _baseUrl <$> getConfig
    dbUpdate (RegisterAction (Action'Edit ed)
                mbIP time baseUrl mbReferrer mbUA)

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
    Edit'SetCategoryProsConsEnabled catId _ _ ->
        [CacheCategoryInfo catId]
    Edit'SetCategoryEcosystemEnabled catId _ _ ->
        [CacheCategoryInfo catId]
    Edit'SetCategoryNotesEnabled catId _ _ ->
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

-- | Do an action that would undo an edit.
--
-- 'Left' signifies failure.
--
-- This doesn't do cache invalidation (you have to do it at the call site
-- using 'invalidateCacheForEdit').
--
-- TODO: make this do cache invalidation.
--
-- TODO: many of these don't work when the changed category/item/etc has been
-- deleted; this should change.
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
undoEdit (Edit'SetCategoryProsConsEnabled catId old new) = do
  now <- view prosConsEnabled <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "pros-cons-enabled has been changed further")
    else Right () <$ dbUpdate (SetCategoryProsConsEnabled catId old)
undoEdit (Edit'SetCategoryEcosystemEnabled catId old new) = do
  now <- view ecosystemEnabled <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "ecosystem-enabled has been changed further")
    else Right () <$ dbUpdate (SetCategoryEcosystemEnabled catId old)
undoEdit (Edit'SetCategoryNotesEnabled catId old new) = do
  now <- view notesEnabled <$> dbQuery (GetCategory catId)
  if now /= new
    then return (Left "notes-enabled has been changed further")
    else Right () <$ dbUpdate (SetCategoryNotesEnabled catId old)
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

renderMethods :: SpockM () () ServerState ()
renderMethods = Spock.subcomponent "render" $ do
  -- Notes for a category
  Spock.get (categoryVar <//> "notes") $ \catId -> do
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderCategoryNotes category
  -- Item colors
  Spock.get (itemVar <//> "colors") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    let hue = getItemHue category item
    json $ M.fromList [("light" :: Text, hueToLightColor hue),
                       ("dark" :: Text, hueToDarkColor hue)]
  -- Item info
  Spock.get (itemVar <//> "info") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemInfo category item
  -- Item description
  Spock.get (itemVar <//> "description") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemDescription item
  -- Item ecosystem
  Spock.get (itemVar <//> "ecosystem") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    lucidIO $ renderItemEcosystem item
  -- Item notes
  Spock.get (itemVar <//> "notes") $ \itemId -> do
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemNotes category item

setMethods :: SpockM () () ServerState ()
setMethods = Spock.subcomponent "set" $ do
  Spock.post (categoryVar <//> "info") $ \catId -> do
    -- TODO: [easy] add a cross-link saying where the form is handled in the
    -- code and other notes saying where stuff is rendered, etc
    invalidateCache' (CacheCategoryInfo catId)
    title' <- T.strip <$> param' "title"
    group' <- T.strip <$> param' "group"
    prosConsEnabled'  <- (Just ("on" :: Text) ==) <$>
                         param "pros-cons-enabled"
    ecosystemEnabled' <- (Just ("on" :: Text) ==) <$>
                         param "ecosystem-enabled"
    notesEnabled'     <- (Just ("on" :: Text) ==) <$>
                         param "notes-enabled"
    status' <- do
      statusName :: Text <- param' "status"
      return $ case statusName of
        "finished" -> CategoryFinished
        "wip"      -> CategoryWIP
        "stub"     -> CategoryStub
        other      -> error ("unknown category status: " ++ show other)
    -- Modify the category
    -- TODO: actually validate the form and report errors
    unless (T.null title') $ do
      (edit, _) <- dbUpdate (SetCategoryTitle catId title')
      addEdit edit
    unless (T.null group') $ do
      (edit, _) <- dbUpdate (SetCategoryGroup catId group')
      addEdit edit
    do (edit, _) <- dbUpdate (SetCategoryStatus catId status')
       addEdit edit
    do (edit, _) <- dbUpdate $
                    SetCategoryProsConsEnabled catId prosConsEnabled'
       addEdit edit
    do (edit, _) <- dbUpdate $
                    SetCategoryEcosystemEnabled catId ecosystemEnabled'
       addEdit edit
    do (edit, _) <- dbUpdate $
                    SetCategoryNotesEnabled catId notesEnabled'
       addEdit edit
    -- After all these edits we can render the category header
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderCategoryInfo category
  -- Notes for a category
  Spock.post (categoryVar <//> "notes") $ \catId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (notes.mdText) <$> dbQuery (GetCategory catId)
    if modified == original
      then do
        invalidateCache' (CacheCategoryNotes catId)
        (edit, category) <- dbUpdate (SetCategoryNotes catId content')
        addEdit edit
        lucidIO $ renderCategoryNotes category
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Item info
  Spock.post (itemVar <//> "info") $ \itemId -> do
    -- TODO: [easy] add a cross-link saying where the form is handled in the
    -- code and other notes saying where stuff is rendered, etc
    invalidateCache' (CacheItemInfo itemId)
    name' <- T.strip <$> param' "name"
    link' <- T.strip <$> param' "link"
    kind' <- do
      kindName :: Text <- param' "kind"
      hackageName' <- (\x -> if T.null x then Nothing else Just x) <$>
                      param' "hackage-name"
      return $ case kindName of
        "library" -> Library hackageName'
        "tool"    -> Tool hackageName'
        _         -> Other
    group' <- do
      groupField <- param' "group"
      customGroupField <- param' "custom-group"
      return $ case groupField of
        "-" -> Nothing
        ""  -> Just customGroupField
        _   -> Just groupField
    -- Modify the item
    -- TODO: actually validate the form and report errors
    --       (don't forget to check that custom-group ≠ "")
    unless (T.null name') $ do
      (edit, _) <- dbUpdate (SetItemName itemId name')
      addEdit edit
    case (T.null link', sanitiseUrl link') of
      (True, _) -> do
          (edit, _) <- dbUpdate (SetItemLink itemId Nothing)
          addEdit edit
      (_, Just l) -> do
          (edit, _) <- dbUpdate (SetItemLink itemId (Just l))
          addEdit edit
      _otherwise ->
          return ()
    do (edit, _) <- dbUpdate (SetItemKind itemId kind')
       addEdit edit
    -- This does all the work of assigning new colors, etc. automatically
    do (edit, _) <- dbUpdate (SetItemGroup itemId group')
       addEdit edit
    -- After all these edits we can render the item
    item <- dbQuery (GetItem itemId)
    category <- dbQuery (GetCategoryByItem itemId)
    lucidIO $ renderItemInfo category item
  -- Item description
  Spock.post (itemVar <//> "description") $ \itemId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (description.mdText) <$> dbQuery (GetItem itemId)
    if modified == original
      then do
        invalidateCache' (CacheItemDescription itemId)
        (edit, item) <- dbUpdate (SetItemDescription itemId content')
        addEdit edit
        lucidIO $ renderItemDescription item
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Item ecosystem
  Spock.post (itemVar <//> "ecosystem") $ \itemId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (ecosystem.mdText) <$> dbQuery (GetItem itemId)
    if modified == original
      then do
        invalidateCache' (CacheItemEcosystem itemId)
        (edit, item) <- dbUpdate (SetItemEcosystem itemId content')
        addEdit edit
        lucidIO $ renderItemEcosystem item
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Item notes
  Spock.post (itemVar <//> "notes") $ \itemId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (notes.mdText) <$> dbQuery (GetItem itemId)
    if modified == original
      then do
        invalidateCache' (CacheItemNotes itemId)
        (edit, item) <- dbUpdate (SetItemNotes itemId content')
        addEdit edit
        category <- dbQuery (GetCategoryByItem itemId)
        lucidIO $ renderItemNotes category item
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]
  -- Trait
  Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
    original <- param' "original"
    content' <- param' "content"
    modified <- view (content.mdText) <$> dbQuery (GetTrait itemId traitId)
    if modified == original
      then do
        invalidateCache' (CacheItemTraits itemId)
        (edit, trait) <- dbUpdate (SetTraitContent itemId traitId content')
        addEdit edit
        lucidIO $ renderTrait itemId trait
      else do
        setStatus HTTP.status409
        json $ M.fromList [
          ("modified" :: Text, modified),
          ("merged" :: Text, merge original content' modified)]

addMethods :: SpockM () () ServerState ()
addMethods = Spock.subcomponent "add" $ do
  -- New category
  Spock.post "category" $ do
    title' <- param' "content"
    -- If the category exists already, don't create it
    cats <- view categories <$> dbQuery GetGlobalState
    let hasSameTitle cat = T.toCaseFold (cat^.title) == T.toCaseFold title'
    category <- case find hasSameTitle cats of
      Just c  -> return c
      Nothing -> do
        catId <- randomShortUid
        time <- liftIO getCurrentTime
        (edit, newCategory) <- dbUpdate (AddCategory catId title' time)
        invalidateCache' (CacheCategory catId)
        addEdit edit
        return newCategory
    -- And now send the URL of the new (or old) category
    Spock.text ("/haskell/" <> categorySlug category)

  -- New item in a category
  Spock.post (categoryVar <//> "item") $ \catId -> do
    name' <- param' "name"
    -- TODO: do something if the category doesn't exist (e.g. has been
    -- already deleted)
    itemId <- randomShortUid
    -- If the item name looks like a Hackage library, assume it's a Hackage
    -- library.
    time <- liftIO getCurrentTime
    (edit, newItem) <-
      if T.all (\c -> isAscii c && (isAlphaNum c || c == '-')) name'
        then dbUpdate (AddItem catId itemId name' time (Library (Just name')))
        else dbUpdate (AddItem catId itemId name' time Other)
    invalidateCache' (CacheItem itemId)
    addEdit edit
    category <- dbQuery (GetCategory catId)
    lucidIO $ renderItem category newItem
  -- Pro (argument in favor of an item)
  Spock.post (itemVar <//> "pro") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomLongUid
    (edit, newTrait) <- dbUpdate (AddPro itemId traitId content')
    invalidateCache' (CacheItemTraits itemId)
    addEdit edit
    lucidIO $ renderTrait itemId newTrait
  -- Con (argument against an item)
  Spock.post (itemVar <//> "con") $ \itemId -> do
    content' <- param' "content"
    traitId <- randomLongUid
    (edit, newTrait) <- dbUpdate (AddCon itemId traitId content')
    invalidateCache' (CacheItemTraits itemId)
    addEdit edit
    lucidIO $ renderTrait itemId newTrait

adminMethods :: SpockM () () ServerState ()
adminMethods = Spock.subcomponent "admin" $ do
  -- Accept an edit
  Spock.post ("edit" <//> var <//> "accept") $ \n -> do
    dbUpdate (RemovePendingEdit n)
    return ()
  -- Undo an edit
  Spock.post ("edit" <//> var <//> "undo") $ \n -> do
    (edit, _) <- dbQuery (GetEdit n)
    res <- undoEdit edit
    case res of
      Left err -> Spock.text (T.pack err)
      Right () -> do invalidateCacheForEdit edit
                     dbUpdate (RemovePendingEdit n)
                     Spock.text ""
  -- Accept a range of edits
  Spock.post ("edits" <//> var <//> var <//> "accept") $ \m n -> do
    dbUpdate (RemovePendingEdits m n)
  -- Undo a range of edits
  Spock.post ("edits" <//> var <//> var <//> "undo") $ \m n -> do
    edits <- dbQuery (GetEdits m n)
    s <- dbQuery GetGlobalState
    failed <- fmap catMaybes $ for edits $ \(edit, details) -> do
      res <- undoEdit edit
      case res of
        Left err -> return (Just ((edit, details), Just err))
        Right () -> do invalidateCacheForEdit edit
                       dbUpdate (RemovePendingEdit (editId details))
                       return Nothing
    case failed of
      [] -> Spock.text ""
      _  -> lucidIO $ renderEdits s failed
  -- Create a checkpoint
  Spock.post "create-checkpoint" $ do
    db <- _db <$> Spock.getState
    createCheckpoint' db

otherMethods :: SpockM () () ServerState ()
otherMethods = do
  -- Moving things
  Spock.subcomponent "move" $ do
    -- Move item
    Spock.post itemVar $ \itemId -> do
      direction :: Text <- param' "direction"
      edit <- dbUpdate (MoveItem itemId (direction == "up"))
      invalidateCache' (CacheItem itemId)
      addEdit edit
    -- Move trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      direction :: Text <- param' "direction"
      edit <- dbUpdate (MoveTrait itemId traitId (direction == "up"))
      invalidateCache' (CacheItemTraits itemId)
      addEdit edit

  -- Deleting things
  Spock.subcomponent "delete" $ do
    -- Delete category
    Spock.post categoryVar $ \catId -> do
      invalidateCache' (CacheCategory catId)
      mbEdit <- dbUpdate (DeleteCategory catId)
      mapM_ addEdit mbEdit
    -- Delete item
    Spock.post itemVar $ \itemId -> do
      invalidateCache' (CacheItem itemId)
      mbEdit <- dbUpdate (DeleteItem itemId)
      mapM_ addEdit mbEdit
    -- Delete trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      invalidateCache' (CacheItemTraits itemId)
      mbEdit <- dbUpdate (DeleteTrait itemId traitId)
      mapM_ addEdit mbEdit

  -- Feeds
  -- TODO: this link shouldn't be absolute [absolute-links]
  baseUrl <- (</> "haskell") . T.unpack . _baseUrl <$> getConfig
  Spock.subcomponent "feed" $ do
    -- Feed for items in a category
    Spock.get categoryVar $ \catId -> do
      category <- dbQuery (GetCategory catId)
      let sortedItems = reverse $ sortBy cmp (category^.items)
            where cmp = comparing (^.created) <> comparing (^.uid)
      let route = "feed" <//> categoryVar
      -- We use ++ instead of </> because the rendered route already has ‘/’
      -- in front of it, and if we used </> it'd just skip baseUrl
      let feedUrl = baseUrl ++ T.unpack (renderRoute route (category^.uid))
          feedTitle = Atom.TextString (T.unpack (category^.title) ++
                                       " – Haskell – Aelve Guide")
          feedLastUpdate = case sortedItems of
            (item:_) -> Feed.toFeedDateStringUTC Feed.AtomKind (item^.created)
            _        -> ""
      let feedBase = Atom.nullFeed feedUrl feedTitle feedLastUpdate
      entries <- liftIO $ mapM (itemToFeedEntry baseUrl category) sortedItems
      atomFeed $ feedBase {
        Atom.feedEntries = entries,
        Atom.feedLinks   = [Atom.nullLink feedUrl] }

itemToFeedEntry
  :: (MonadIO m)
  => String -> Category -> Item -> m Atom.Entry
itemToFeedEntry baseUrl category item = do
  entryContent <- Lucid.renderTextT (renderItemForFeed category item)
  return entryBase {
    Atom.entryLinks = [Atom.nullLink entryLink],
    Atom.entryContent = Just (Atom.HTMLContent (TL.unpack entryContent)) }
  where
    entryLink = baseUrl </>
                T.unpack (T.format "{}#item-{}"
                                   (categorySlug category, item^.uid))
    entryBase = Atom.nullEntry
      (T.unpack (uidToText (item^.uid)))
      (Atom.TextString (T.unpack (item^.name)))
      (Feed.toFeedDateStringUTC Feed.AtomKind (item^.created))

-- TODO: rename GlobalState to DB, and DB to AcidDB

lucidWithConfig
  :: (MonadIO m, HasSpock (ActionCtxT cxt m),
      SpockState (ActionCtxT cxt m) ~ ServerState)
  => HtmlT (ReaderT Config IO) a -> ActionCtxT cxt m a
lucidWithConfig x = do
  cfg <- getConfig
  lucidIO (hoist (flip runReaderT cfg) x)

-- | Like 'createCheckpoint', but doesn't create a checkpoint if there were
-- no changes made.
createCheckpoint' :: MonadIO m => DB -> m ()
createCheckpoint' db = liftIO $ do
  wasDirty <- Acid.update db UnsetDirty
  when wasDirty $ do
    createArchive db
    createCheckpoint db

----------------------------------------------------------------------------
-- The entry point
----------------------------------------------------------------------------

-- | Start the site.
main :: IO ()
main = do
  config <- readConfig
  mainWith config

-- | Start the site with a specific 'Config'.
mainWith :: Config -> IO ()
mainWith config = do
  -- Emptying the cache is needed because during development (i.e. in REPL)
  -- 'main' can be started many times and if the cache isn't cleared changes
  -- won't be visible
  emptyCache
  startTemplateWatcher
  let emptyState = GlobalState {
        _categories = [],
        _categoriesDeleted = [],
        _actions = [],
        _pendingEdits = [],
        _editIdCounter = 0,
        _dirty = True }
  do args <- getArgs
     when (args == ["--dry-run"]) $ do
       db :: DB <- openLocalStateFrom "state/" (error "couldn't load state")
       putStrLn "loaded the database successfully"
       closeAcidState db
       exitSuccess
  -- When we run in GHCi and we exit the main thread, the EKG thread (that
  -- runs the localhost:5050 server which provides statistics) may keep
  -- running. This makes running this in GHCi annoying, because you have to
  -- restart GHCi before every run. So, we kill the thread in the finaliser.
  ekgId <- newIORef Nothing
  -- See Note [acid-state] for the explanation of 'openLocalStateFrom',
  -- 'createCheckpoint', etc
  let prepare = openLocalStateFrom "state/" emptyState
      finalise db = do
        createCheckpoint' db
        closeAcidState db
        mapM_ killThread =<< readIORef ekgId
  bracket prepare finalise $ \db -> do
    hSetBuffering stdout NoBuffering
    -- Create a checkpoint every six hours. Note: if nothing was changed, the
    -- checkpoint won't be created, which saves us some space.
    Slave.fork $ forever $ do
      createCheckpoint' db
      threadDelay (1000000 * 3600 * 6)
    -- EKG metrics
    ekg <- EKG.forkServer "localhost" 5050
    writeIORef ekgId (Just (EKG.serverThreadId ekg))
    waiMetrics <- EKG.registerWaiMetrics (EKG.serverMetricStore ekg)
    categoryGauge <- EKG.getGauge "db.categories" ekg
    itemGauge <- EKG.getGauge "db.items" ekg
    Slave.fork $ forever $ do
      globalState <- Acid.query db GetGlobalState
      let allCategories = globalState^.categories
      let allItems = allCategories^..each.items.each
      EKG.Gauge.set categoryGauge (fromIntegral (length allCategories))
      EKG.Gauge.set itemGauge (fromIntegral (length allItems))
      threadDelay (1000000 * 60)
    -- Run the server
    let serverState = ServerState {
          _config = config,
          _db     = db }
    spockConfig <- do
      cfg <- defaultSpockCfg () PCNoDatabase serverState
      return cfg {
        spc_maxRequestSize = Just (1024*1024) }
    when (_prerender config) $ prerenderPages config db
    runSpock 8080 $ spock spockConfig $ do
      middleware (EKG.metrics waiMetrics)
      middleware (staticPolicy (addBase "static"))
      -- Javascript
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        js <- getJS
        Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions <> js)
      -- CSS
      Spock.get "/highlight.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (T.pack (styleToCss pygments))
      Spock.get "/css.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        css <- getCSS
        Spock.bytes $ T.encodeUtf8 css
      Spock.get "/admin.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        css <- getCSS
        admincss <- liftIO $ T.readFile "static/admin.css"
        Spock.bytes $ T.encodeUtf8 (css <> admincss)

      -- Main page
      Spock.get root $
        lucidWithConfig $ renderRoot

      -- Admin page
      prehook adminHook $ do
        Spock.get "admin" $ do
          s <- dbQuery GetGlobalState
          lucidIO $ renderAdmin s
        adminMethods

      -- Donation page
      Spock.get "donate" $
        lucidWithConfig $ renderDonate

      -- Static pages
      Spock.get "unwritten-rules" $ lucidWithConfig $
        renderStaticMd "Unwritten rules" "unwritten-rules.md"
      Spock.get "markdown" $ lucidWithConfig $
        renderStaticMd "Markdown" "markdown.md"
      Spock.get "license" $ lucidWithConfig $
        renderStaticMd "License" "license.md"

      -- Haskell
      Spock.subcomponent "haskell" $ do
        Spock.get root $ do
          s <- dbQuery GetGlobalState
          q <- param "q"
          (time, mbIP, mbReferrer, mbUA) <- getDetails
          let act = case q of
                Nothing -> Action'MainPageVisit
                Just x  -> Action'Search x
          baseUrl <- _baseUrl <$> getConfig
          dbUpdate (RegisterAction act mbIP time baseUrl mbReferrer mbUA)
          lucidWithConfig $ renderHaskellRoot s q
        -- Category pages
        Spock.get var $ \path -> do
          -- The links look like /parsers-gao238b1 (because it's nice when
          -- you can find out where a link leads just by looking at it)
          let (_, catId) = T.breakOnEnd "-" path
          when (T.null catId) $
            Spock.jumpNext
          mbCategory <- dbQuery (GetCategoryMaybe (Uid catId))
          case mbCategory of
            Nothing -> Spock.jumpNext
            Just category -> do
              (time, mbIP, mbReferrer, mbUA) <- getDetails
              baseUrl <- _baseUrl <$> getConfig
              dbUpdate $ RegisterAction (Action'CategoryVisit (Uid catId))
                           mbIP time baseUrl mbReferrer mbUA
              -- If the slug in the url is old (i.e. if it doesn't match the
              -- one we would've generated now), let's do a redirect
              when (categorySlug category /= path) $
                -- TODO: this link shouldn't be absolute [absolute-links]
                Spock.redirect ("/haskell/" <> categorySlug category)
              lucidWithConfig $ renderCategoryPage category
        -- The add/set methods return rendered parts of the structure (added
        -- categories, changed items, etc) so that the Javascript part could
        -- take them and inject into the page. We don't want to duplicate
        -- rendering on server side and on client side.
        renderMethods
        setMethods
        addMethods
        otherMethods

adminHook :: ActionCtxT ctx (WebStateM () () ServerState) ()
adminHook = do
  adminPassword <- _adminPassword <$> getConfig
  unless (adminPassword == "") $ do
    let check user pass =
          unless (user == "admin" && pass == adminPassword) $ do
            Spock.setStatus HTTP.status401
            Spock.text "Wrong password!"
    Spock.requireBasicAuth "Authenticate (login = admin)" check return

-- TODO: a function to find all links to Hackage that have version in them

-- | During development you need to see the changes whenever you change
-- anything. This function starts a thread that watches for changes in
-- templates and clears the cache whenever a change occurs, so that you
-- wouldn't see cached pages.
startTemplateWatcher :: IO ()
startTemplateWatcher = void $ do
  Slave.fork $ FSNotify.withManager $ \mgr -> do
    FSNotify.watchTree mgr "templates/" (const True) $ \_ -> do
      emptyCache
    forever $ threadDelay 1000000

-- | Render all pages and put them into the cache, so that (unlucky) users
-- wouldn't see delays after a restart of the site.
--
-- Well, actually instead unlucky users would see an error after a restart of
-- the site until prerendering completes, which is probably worse.
--
-- TODO: make prerendering asynchronous.
prerenderPages :: Config -> DB -> IO ()
prerenderPages config db = do
  putStr "Prerendering pages to be cached... "
  globalState <- Acid.query db GetGlobalState
  for_ (globalState^.categories) $ \cat -> do
    putStr "|"
    evaluate . force =<<
      renderBST (hoist (flip runReaderT config) (renderCategoryPage cat))
  putStrLn " done"
