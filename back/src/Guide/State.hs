{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Site's database, and methods for manipulating it.
module Guide.State
(
  DB,
  withDB,
  createCheckpoint',
  createCheckpointAndClose',

  -- * type of global state
  GlobalState(..),
  GlobalStateLenses(..),
  findCategoryByItem,
  emptyState,

  -- * acid-state methods
  -- ** query
  GetGlobalState(..),
  GetCategories(..),
  GetCategory(..), GetCategoryMaybe(..),
  GetCategoryByItem(..),
  GetItem(..), GetItemMaybe (..),
  GetTrait(..), GetTraitMaybe (..),

  -- ** add
  AddCategory(..),
  AddItem(..),
  AddPro(..),
  AddCon(..),

  -- ** set
  SetGlobalState(..),
  -- *** 'Category'
  SetCategoryTitle(..),
  SetCategoryGroup(..),
  SetCategoryNotes(..),
  SetCategoryStatus(..),
  ChangeCategoryEnabledSections(..),
  -- *** 'Item'
  SetItemName(..),
  SetItemLink(..),
  SetItemHackage(..),
  SetItemSummary(..),
  SetItemNotes(..),
  SetItemEcosystem(..),
  -- *** 'Trait'
  SetTraitContent(..),

  -- ** delete
  DeleteCategory(..),
  DeleteItem(..),
  DeleteTrait(..),

  -- ** edits
  GetEdit(..), GetEdits(..),
  RegisterEdit(..),
  RemovePendingEdit(..), RemovePendingEdits(..),

  -- ** other
  MoveItem(..),
  MoveTrait(..),
  RestoreCategory(..),
  RestoreItem(..),
  RestoreTrait(..),
  SetDirty(..), UnsetDirty(..),

  LoadSession(..), StoreSession(..),
  DeleteSession(..), GetSessions(..),

  GetUser(..), CreateUser(..), DeleteUser(..), DeleteAllUsers(..),
  LoginUser(..),

  GetAdminUsers(..),

  -- * PublicDB
  PublicDB(..),
  toPublicDB,
  fromPublicDB,
  -- ** queries
  ImportPublicDB(..),
  ExportPublicDB(..),
)
where


import Imports

-- Network
import Data.IP
-- acid-state
import Data.Acid as Acid
import Data.Acid.Local as Acid
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate
--
import Web.Spock.Internal.SessionManager (SessionId)

import Guide.Markdown
import Guide.Types.Analytics
import Guide.Types.Core
import Guide.Types.Edit
import Guide.Types.Session
import Guide.Types.User
import Guide.Utils

import qualified Data.Map as M
import qualified Data.Set as S


{- Note [extending types]
~~~~~~~~~~~~~~~~~~~~~~~~~

Here's what you should do if you add a new field to 'Trait', 'Item', or
'Category'.


Types.hs
~~~~~~~~~~~~~~~~~~~~~~~~~

  1. Fix all warnings about uninitialised fields that might appear (by
     e.g. providing a default value).

  2. Update the migration code; see Note [acid-state].

  3. If the field is user-editable: add a new constructor to 'Edit' and
     update the migration code for 'Edit'. Update 'isVacuousEdit', too.

  4. Create a method for updating the field (setSomethingField), add it to
     the “makeAcidic ''GlobalState” declaration, and export the
     SetSomethingField type.

  5. Export a lens for the field (if it shares the name with some other
     field, move it to the “* Overloaded things” heading).


JS.hs
~~~~~~~~~~~~~~~~~~~~~~~~~

  1. If the field is user-editable, add a method for setting it and don't
     forget to add it to the 'allJSFunctions' list.


View.hs
~~~~~~~~~~~~~~~~~~~~~~~~~

  1. If the field is non-trivial, add a method for rendering it.

  2. Don't forget to actually render it if the user is supposed to see it.

  3. Add a branch for the constructor you made in Types.hs/#3 to 'renderEdit'.


Guide.hs
~~~~~~~~~~~~~~~~~~~~~~~~~

  1. Add a case to 'undoEdit'.

  2. If the field is user-editable, add a method for changing it to
     'setMethods'.

-}


----------------------------------------------------------------------------
-- GlobalState
----------------------------------------------------------------------------

emptyState :: GlobalState
emptyState = GlobalState {
  categories = [],
  categoriesDeleted = [],
  pendingEdits = [],
  editIdCounter = 0,
  sessionStore = M.empty,
  users = M.empty,
  dirty = True }

data GlobalState = GlobalState {
  categories        :: [Category],
  categoriesDeleted :: [Category],
  -- | Pending edits, newest first
  pendingEdits      :: [(Edit, EditDetails)],
  -- | ID of next edit that will be made
  editIdCounter     :: Int,
  -- | Sessions
  sessionStore      :: Map SessionId GuideSession,
  -- | Users
  users             :: Map (Uid User) User,
  -- | The dirty bit (needed to choose whether to make a checkpoint or not)
  dirty             :: Bool }
  deriving (Show)

deriveSafeCopySorted 9 'extension ''GlobalState
makeClassWithLenses ''GlobalState

changelog ''GlobalState (Current 9, Past 8) [
  -- TODO: it's silly that we have to reference 'Action' and keep it in the
  -- codebase even though we have no use for 'Action' anymore
  Removed "actions" [t|[(Action, ActionDetails)]|]
  ]
deriveSafeCopySorted 8 'extension ''GlobalState_v8

changelog ''GlobalState (Past 8, Past 7) [
  Added "sessionStore" [hs|M.empty|],
  Added "users" [hs|M.empty|]
  ]
deriveSafeCopySorted 7 'base ''GlobalState_v7

traitById :: Uid Trait -> Lens' Item Trait
traitById traitId = singular $
  maybeTraitById traitId `failing`
  error ("traitById: couldn't find trait with uid " ++
         toString (uidToText traitId))

maybeTraitById :: Uid Trait -> Traversal' Item Trait
maybeTraitById traitId =
  (_itemPros . each . filtered ((== traitId) . traitUid)) `failing`
  (_itemCons . each . filtered ((== traitId) . traitUid))

categoryById :: Uid Category -> Lens' GlobalState Category
categoryById catId = singular $
  maybeCategoryById catId `failing`
  error ("categoryById: couldn't find category with uid " ++
         toString (uidToText catId))

maybeCategoryById :: Uid Category -> Traversal' GlobalState Category
maybeCategoryById catId = _categories . each . filtered ((== catId) . categoryUid)

itemById :: Uid Item -> Lens' GlobalState Item
itemById itemId = singular $
  maybeItemById itemId `failing`
  error ("itemById: couldn't find item with uid " ++
         toString (uidToText itemId))

maybeItemById :: Uid Item -> Traversal' GlobalState Item
maybeItemById itemId =
  _categories . each . _categoryItems . each . filtered ((== itemId) . itemUid)

findCategoryByItem :: Uid Item -> GlobalState -> Category
findCategoryByItem itemId s =
  fromMaybe (error err) (find hasItem (categories s))
  where
    err = "findCategoryByItem: couldn't find category with item with uid " ++
          toString (uidToText itemId)
    hasItem category = itemId `elem` (category ^.. _categoryItems . each . _itemUid)

-- | 'PublicDB' contains all safe data from 'GlobalState'.
-- Difference from 'GlobalState':
-- * 'User' replaced with 'PublicUser'
-- * Sessions information removed
-- * Dirty flag removed
data PublicDB = PublicDB {
  publicCategories        :: [Category],
  publicCategoriesDeleted :: [Category],
  publicPendingEdits      :: [(Edit, EditDetails)],
  publicEditIdCounter     :: Int,
  publicUsers             :: Map (Uid User) PublicUser}
  deriving (Show)

-- NOTE: you don't need to write migrations for 'PublicDB' but you still
-- need to increase the version when the type changes, so that old clients
-- wouldn't get cryptic error messages like “not enough bytes” when trying
-- to deserialize a new version of 'PublicDB' that they can't handle.
deriveSafeCopySorted 1 'base ''PublicDB

-- | Converts 'GlobalState' to 'PublicDB' type stripping private data.
toPublicDB :: GlobalState -> PublicDB
toPublicDB $(fields 'GlobalState) =
  PublicDB {
    publicCategories        = categories,
    publicCategoriesDeleted = categoriesDeleted,
    publicPendingEdits      = pendingEdits,
    publicEditIdCounter     = editIdCounter,
    publicUsers             = fmap userToPublic users
  }
  where
    -- Ignored fields
    _ = (dirty, sessionStore)

-- | Converts 'PublicDB' to 'GlobalState' type filling in non-existing data with
-- default values.
fromPublicDB :: PublicDB -> GlobalState
fromPublicDB $(fields 'PublicDB) =
  GlobalState {
    categories        = publicCategories,
    categoriesDeleted = publicCategoriesDeleted,
    pendingEdits      = publicPendingEdits,
    editIdCounter     = publicEditIdCounter,
    sessionStore      = M.empty,
    users             = fmap publicUserToUser publicUsers,
    dirty             = True
  }

-- get

getGlobalState :: Acid.Query GlobalState GlobalState
getGlobalState = view id

getCategories :: Acid.Query GlobalState [Category]
getCategories = view _categories

getCategory :: Uid Category -> Acid.Query GlobalState Category
getCategory uid' = view (categoryById uid')

getCategoryMaybe :: Uid Category -> Acid.Query GlobalState (Maybe Category)
getCategoryMaybe uid' = preview (maybeCategoryById uid')

getCategoryByItem :: Uid Item -> Acid.Query GlobalState Category
getCategoryByItem uid' = findCategoryByItem uid' <$> ask

getItem :: Uid Item -> Acid.Query GlobalState Item
getItem uid' = view (itemById uid')

getItemMaybe :: Uid Item -> Acid.Query GlobalState (Maybe Item)
getItemMaybe uid' = preview (maybeItemById uid')

-- TODO: this doesn't need the item id, but then we have to be a bit cleverer
-- and store a (TraitId -> ItemId) map in global state (and update it
-- accordingly whenever anything happens, so perhaps let's not do it!)
getTrait :: Uid Item -> Uid Trait -> Acid.Query GlobalState Trait
getTrait itemId traitId = view (itemById itemId . traitById traitId)

getTraitMaybe :: Uid Item -> Uid Trait -> Acid.Query GlobalState (Maybe Trait)
getTraitMaybe itemId traitId = preview (maybeItemById itemId . maybeTraitById traitId)

-- add

addCategory
  :: Uid Category    -- ^ New category's id
  -> Text            -- ^ Title
  -> Text            -- ^ Group
  -> UTCTime         -- ^ Creation time
  -> Acid.Update GlobalState (Edit, Category)
addCategory catId title' group' created' = do
  let newCategory = Category {
        categoryUid = catId,
        categoryTitle = title',
        categoryGroup = group',
        categoryEnabledSections = S.fromList [
            ItemProsConsSection,
            ItemEcosystemSection,
            ItemNotesSection ],
        categoryCreated = created',
        categoryStatus = CategoryStub,
        categoryNotes = toMarkdownBlock "",
        categoryItems = [],
        categoryItemsDeleted = [] }
  _categories %= (newCategory :)
  let edit = Edit'AddCategory catId title' group'
  return (edit, newCategory)

addItem
  :: Uid Category    -- ^ Category id
  -> Uid Item        -- ^ New item's id
  -> Text            -- ^ Name
  -> UTCTime         -- ^ Creation time
  -> Acid.Update GlobalState (Edit, Item)
addItem catId itemId name' created' = do
  let newItem = Item {
        itemUid         = itemId,
        itemName        = name',
        itemCreated     = created',
        itemHackage     = Nothing,
        itemSummary     = toMarkdownBlock "",
        itemPros        = [],
        itemProsDeleted = [],
        itemCons        = [],
        itemConsDeleted = [],
        itemEcosystem   = toMarkdownBlock "",
        itemNotes       = let pref = "item-notes-" <> uidToText itemId <> "-"
                          in  toMarkdownTree pref "",
        itemLink        = Nothing}
  categoryById catId . _categoryItems %= (++ [newItem])
  let edit = Edit'AddItem catId itemId name'
  return (edit, newItem)

addPro
  :: Uid Item       -- ^ Item id
  -> Uid Trait      -- ^ New trait's id
  -> Text
  -> Acid.Update GlobalState (Edit, Trait)
addPro itemId traitId text' = do
  let newTrait = Trait traitId (toMarkdownInline text')
  itemById itemId . _itemPros %= (++ [newTrait])
  let edit = Edit'AddPro itemId traitId text'
  return (edit, newTrait)

addCon
  :: Uid Item       -- ^ Item id
  -> Uid Trait      -- ^ New trait's id
  -> Text
  -> Acid.Update GlobalState (Edit, Trait)
addCon itemId traitId text' = do
  let newTrait = Trait traitId (toMarkdownInline text')
  itemById itemId . _itemCons %= (++ [newTrait])
  let edit = Edit'AddCon itemId traitId text'
  return (edit, newTrait)

-- set

-- Almost all of these return an 'Edit' that corresponds to the edit that has
-- been performed.

-- | Can be useful sometimes (e.g. if you want to regenerate all uids), but
-- generally shouldn't be used.
setGlobalState :: GlobalState -> Acid.Update GlobalState ()
setGlobalState = (id .=)

setCategoryTitle :: Uid Category -> Text -> Acid.Update GlobalState (Edit, Category)
setCategoryTitle catId title' = do
  oldTitle <- categoryById catId . _categoryTitle <<.= title'
  let edit = Edit'SetCategoryTitle catId oldTitle title'
  (edit,) <$> use (categoryById catId)

setCategoryGroup :: Uid Category -> Text -> Acid.Update GlobalState (Edit, Category)
setCategoryGroup catId group' = do
  oldGroup <- categoryById catId . _categoryGroup <<.= group'
  let edit = Edit'SetCategoryGroup catId oldGroup group'
  (edit,) <$> use (categoryById catId)

setCategoryNotes :: Uid Category -> Text -> Acid.Update GlobalState (Edit, Category)
setCategoryNotes catId notes' = do
  oldNotes <- categoryById catId . _categoryNotes <<.= toMarkdownBlock notes'
  let edit = Edit'SetCategoryNotes catId (markdownBlockSource oldNotes) notes'
  (edit,) <$> use (categoryById catId)

setCategoryStatus :: Uid Category -> CategoryStatus -> Acid.Update GlobalState (Edit, Category)
setCategoryStatus catId status' = do
  oldStatus <- categoryById catId . _categoryStatus <<.= status'
  let edit = Edit'SetCategoryStatus catId oldStatus status'
  (edit,) <$> use (categoryById catId)

changeCategoryEnabledSections
  :: Uid Category
  -> Set ItemSection     -- ^ Sections to enable
  -> Set ItemSection     -- ^ Sections to disable
  -> Acid.Update GlobalState (Edit, Category)
changeCategoryEnabledSections catId toEnable toDisable = do
  categoryById catId . _categoryEnabledSections %= \sections ->
    (sections <> toEnable) S.\\ toDisable
  let edit = Edit'ChangeCategoryEnabledSections catId toEnable toDisable
  (edit,) <$> use (categoryById catId)

setItemName :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemName itemId name' = do
  oldName <- itemById itemId . _itemName <<.= name'
  let edit = Edit'SetItemName itemId oldName name'
  (edit,) <$> use (itemById itemId)

setItemLink :: Uid Item -> Maybe Url -> Acid.Update GlobalState (Edit, Item)
setItemLink itemId link' = do
  oldLink <- itemById itemId . _itemLink <<.= link'
  let edit = Edit'SetItemLink itemId oldLink link'
  (edit,) <$> use (itemById itemId)

setItemHackage :: Uid Item -> Maybe Text -> Acid.Update GlobalState (Edit, Item)
setItemHackage itemId hackage' = do
    oldName <- itemById itemId . _itemHackage <<.= hackage'
    let edit = Edit'SetItemHackage itemId oldName hackage'
    (edit,) <$> use (itemById itemId)

setItemSummary :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemSummary itemId description' = do
  oldDescr <- itemById itemId . _itemSummary <<.=
                toMarkdownBlock description'
  let edit = Edit'SetItemSummary itemId
               (markdownBlockSource oldDescr) description'
  (edit,) <$> use (itemById itemId)

setItemNotes :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemNotes itemId notes' = do
  let pref = "item-notes-" <> uidToText itemId <> "-"
  oldNotes <- itemById itemId . _itemNotes <<.= toMarkdownTree pref notes'
  let edit = Edit'SetItemNotes itemId (markdownTreeSource oldNotes) notes'
  (edit,) <$> use (itemById itemId)

setItemEcosystem :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemEcosystem itemId ecosystem' = do
  oldEcosystem <- itemById itemId . _itemEcosystem <<.= toMarkdownBlock ecosystem'
  let edit = Edit'SetItemEcosystem itemId
               (markdownBlockSource oldEcosystem) ecosystem'
  (edit,) <$> use (itemById itemId)

setTraitContent :: Uid Item -> Uid Trait -> Text -> Acid.Update GlobalState (Edit, Trait)
setTraitContent itemId traitId content' = do
  oldContent <- itemById itemId . traitById traitId . _traitContent <<.=
                  toMarkdownInline content'
  let edit = Edit'SetTraitContent itemId traitId
               (markdownInlineSource oldContent) content'
  (edit,) <$> use (itemById itemId . traitById traitId)

-- delete

deleteCategory :: Uid Category -> Acid.Update GlobalState (Either String Edit)
deleteCategory catId = do
  mbCategory <- preuse (categoryById catId)
  case mbCategory of
    Nothing       -> return (Left "category not found")
    Just category -> do
      mbCategoryPos <- findIndex ((== catId) . categoryUid) <$> use _categories
      case mbCategoryPos of
        Nothing          -> return (Left "category not found")
        Just categoryPos -> do
          _categories %= deleteAt categoryPos
          _categoriesDeleted %= (category:)
          return (Right (Edit'DeleteCategory catId categoryPos))

deleteItem :: Uid Item -> Acid.Update GlobalState (Either String Edit)
deleteItem itemId = do
  catId <- categoryUid . findCategoryByItem itemId <$> get
  let categoryLens :: Lens' GlobalState Category
      categoryLens = categoryById catId
  let itemLens :: Lens' GlobalState Item
      itemLens = itemById itemId
  mbItem <- preuse itemLens
  case mbItem of
    Nothing   -> return (Left "item not found")
    Just item -> do
      allItems <- use (categoryLens . _categoryItems)
      case findIndex ((== itemId) . itemUid) allItems of
        Nothing      -> return (Left "item not found")
        Just itemPos -> do
          categoryLens . _categoryItems        %= deleteAt itemPos
          categoryLens . _categoryItemsDeleted %= (item:)
          return (Right (Edit'DeleteItem itemId itemPos))

deleteTrait :: Uid Item -> Uid Trait -> Acid.Update GlobalState (Either String Edit)
deleteTrait itemId traitId = do
  let itemLens :: Lens' GlobalState Item
      itemLens = itemById itemId
  mbItem <- preuse itemLens
  case mbItem of
    Nothing   -> return (Left "item not found")
    Just item -> do
      -- Determine whether the trait is a pro or a con, and proceed
      -- accordingly
      case (find ((== traitId) . traitUid) (itemPros item),
            find ((== traitId) . traitUid) (itemCons item)) of
        -- It's in neither group, which means it was deleted. Do nothing.
        (Nothing, Nothing) -> return (Left "trait not found")
        -- It's a pro
        (Just trait, _) -> do
          mbTraitPos <-
            findIndex ((== traitId) . traitUid) <$>
            use (itemLens . _itemPros)
          case mbTraitPos of
            Nothing       -> return (Left "trait not found")
            Just traitPos -> do
              itemLens . _itemPros        %= deleteAt traitPos
              itemLens . _itemProsDeleted %= (trait:)
              return (Right (Edit'DeleteTrait itemId traitId traitPos))
        -- It's a con
        (_, Just trait) -> do
          mbTraitPos <-
            findIndex ((== traitId) . traitUid) <$>
            use (itemLens . _itemCons)
          case mbTraitPos of
            Nothing       -> return (Left "trait not found")
            Just traitPos -> do
              itemLens . _itemCons        %= deleteAt traitPos
              itemLens . _itemConsDeleted %= (trait:)
              return (Right (Edit'DeleteTrait itemId traitId traitPos))

-- other methods

moveItem
  :: Uid Item
  -> Bool       -- ^ 'True' means up, 'False' means down
  -> Acid.Update GlobalState Edit
moveItem itemId up = do
  let move = if up then moveUp else moveDown
  catId <- categoryUid . findCategoryByItem itemId <$> get
  categoryById catId . _categoryItems %= move ((== itemId) . itemUid)
  return (Edit'MoveItem itemId up)

moveTrait
  :: Uid Item
  -> Uid Trait
  -> Bool        -- ^ 'True' means up, 'False' means down
  -> Acid.Update GlobalState Edit
moveTrait itemId traitId up = do
  let move = if up then moveUp else moveDown
  -- The trait is only going to be present in one of the lists so let's do it
  -- in each list because we're too lazy to figure out whether it's a pro or
  -- a con
  itemById itemId . _itemPros %= move ((== traitId) . traitUid)
  itemById itemId . _itemCons %= move ((== traitId) . traitUid)
  return (Edit'MoveTrait itemId traitId up)

restoreCategory :: Uid Category -> Int -> Acid.Update GlobalState (Either String ())
restoreCategory catId pos = do
  deleted <- use _categoriesDeleted
  case find ((== catId) . categoryUid) deleted of
    Nothing -> return (Left "category not found in deleted categories")
    Just category -> do
      _categoriesDeleted %= deleteFirst ((== catId) . categoryUid)
      _categories        %= insertOrAppend pos category
      return (Right ())

restoreItem :: Uid Item -> Int -> Acid.Update GlobalState (Either String ())
restoreItem itemId pos = do
  let ourCategory = any ((== itemId) . itemUid) . categoryItemsDeleted
  allCategories <- use (_categories <> _categoriesDeleted)
  case find ourCategory allCategories of
    Nothing -> return (Left "item not found in deleted items")
    Just category -> do
      let item = fromJust (find ((== itemId) . itemUid) (categoryItemsDeleted category))
      let category' = category
            & _categoryItemsDeleted %~ deleteFirst ((== itemId) . itemUid)
            & _categoryItems        %~ insertOrAppend pos item
      _categories        . each . filtered ourCategory .= category'
      _categoriesDeleted . each . filtered ourCategory .= category'
      return (Right ())

restoreTrait :: Uid Item -> Uid Trait -> Int -> Acid.Update GlobalState (Either String ())
restoreTrait itemId traitId pos = do
  let getItems = view (_categoryItems <> _categoryItemsDeleted)
      ourCategory = any ((== itemId) . itemUid) . getItems
  allCategories <- use (_categories <> _categoriesDeleted)
  case find ourCategory allCategories of
    Nothing -> return (Left "item -that the trait belongs to- not found")
    Just category -> do
      let item = fromJust (find ((== itemId) . itemUid) (getItems category))
      case (find ((== traitId) . traitUid) (itemProsDeleted item),
            find ((== traitId) . traitUid) (itemConsDeleted item)) of
        (Nothing, Nothing) ->
          return (Left "trait not found in deleted traits")
        (Just trait, _) -> do
          let item' = item
                & _itemProsDeleted %~ deleteFirst ((== traitId) . traitUid)
                & _itemPros        %~ insertOrAppend pos trait
          let category' = category
                & _categoryItems        . each . filtered ((== itemId) . itemUid) .~ item'
                & _categoryItemsDeleted . each . filtered ((== itemId) . itemUid) .~ item'
          _categories        . each . filtered ourCategory .= category'
          _categoriesDeleted . each . filtered ourCategory .= category'
          return (Right ())
        (_, Just trait) -> do
          let item' = item
                & _itemConsDeleted %~ deleteFirst ((== traitId) . traitUid)
                & _itemCons        %~ insertOrAppend pos trait
          let category' = category
                & _categoryItems        . each . filtered ((== itemId) . itemUid) .~ item'
                & _categoryItemsDeleted . each . filtered ((== itemId) . itemUid) .~ item'
          _categories        . each . filtered ourCategory .= category'
          _categoriesDeleted . each . filtered ourCategory .= category'
          return (Right ())

-- TODO: maybe have a single list of traits with pro/con being signified by
-- something like TraitType? or maybe TraitType could even be a part of the
-- trait itself?

getEdit :: Int -> Acid.Query GlobalState (Edit, EditDetails)
getEdit n = do
  edits <- view _pendingEdits
  case find ((== n) . editId . snd) edits of
    Nothing   -> error ("no edit with id " ++ show n)
    Just edit -> return edit

-- | Returns edits in order from latest to earliest.
getEdits
  :: Int            -- ^ Id of latest edit
  -> Int            -- ^ Id of earliest edit
  -> Acid.Query GlobalState [(Edit, EditDetails)]
getEdits m n =
  filter (\(_, d) -> n <= editId d && editId d <= m) <$> view _pendingEdits

-- | The edit won't be registered if it's vacuous (see 'isVacuousEdit').
registerEdit
  :: Edit
  -> Maybe IP
  -> UTCTime
  -> Acid.Update GlobalState ()
registerEdit ed ip date = do
  id' <- use _editIdCounter
  let details = EditDetails {
        editIP   = ip,
        editDate = date,
        editId   = id' }
  _pendingEdits %= ((ed, details):)
  _editIdCounter += 1

removePendingEdit :: Int -> Acid.Update GlobalState (Edit, EditDetails)
removePendingEdit n = do
  edits <- use _pendingEdits
  case find ((== n) . editId . snd) edits of
    Nothing   -> error ("no edit with id " ++ show n)
    Just edit -> do
      _pendingEdits %= deleteFirst ((== n) . editId . snd)
      return edit

removePendingEdits
  :: Int            -- ^ Id of latest edit
  -> Int            -- ^ Id of earliest edit
  -> Acid.Update GlobalState ()
removePendingEdits m n = do
  _pendingEdits %= filter (\(_, d) -> editId d < n || m < editId d)

setDirty :: Acid.Update GlobalState ()
setDirty = _dirty .= True

unsetDirty :: Acid.Update GlobalState Bool
unsetDirty = _dirty <<.= False

-- | Retrieves a session by 'SessionID'.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
loadSession :: SessionId -> Acid.Query GlobalState (Maybe GuideSession)
loadSession key = view (_sessionStore . at key)

-- | Stores a session object.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
storeSession :: GuideSession -> Acid.Update GlobalState ()
storeSession sess = do
  _sessionStore %= M.insert (sess ^. sess_id) sess
  setDirty

-- | Deletes a session by 'SessionID'.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
deleteSession :: SessionId -> Acid.Update GlobalState ()
deleteSession key = do
  _sessionStore %= M.delete key
  setDirty

-- | Retrieves all sessions.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
getSessions :: Acid.Query GlobalState [GuideSession]
getSessions = do
  m <- view _sessionStore
  return . map snd $ M.toList m

-- | Retrieves a user by their unique identifier.
getUser :: Uid User -> Acid.Query GlobalState (Maybe User)
getUser key = view (_users . at key)

-- | Creates a user, maintaining unique constraints on certain fields.
createUser :: User -> Acid.Update GlobalState Bool
createUser user = do
  m :: [User] <- toList <$> use _users
  if all (canCreateUser user) m
  then do
    _users %= M.insert (userID user) user
    return True
  else
    return False

-- | Remove a user completely. Unsets all user sessions with this user ID.
deleteUser :: Uid User -> Acid.Update GlobalState ()
deleteUser key = do
  _users %= M.delete key
  logoutUserGlobally key
  setDirty

deleteAllUsers :: Acid.Update GlobalState ()
deleteAllUsers = do
  mapM_ logoutUserGlobally . M.keys =<< use _users
  _users .= mempty
  setDirty

-- | Given an email address and a password, return the user if it exists
-- and the password is correct.
loginUser :: Text -> ByteString -> Acid.Query GlobalState (Either String User)
loginUser email password = do
  matches <- filter (\u -> userEmail u == email) . toList <$> view _users
  case matches of
    [user] ->
      if verifyUser user password
      then return $ Right user
      else return $ Left "wrong password"
    [] -> return $ Left "user not found"
    _  -> return $ Left "more than one user found, please contact the admin"

-- | Global logout of all of a user's active sessions
logoutUserGlobally :: Uid User -> Acid.Update GlobalState ()
logoutUserGlobally key = do
  sessions <- use _sessionStore
  for_ (M.toList sessions) $ \(sessID, sess) -> do
    when ((sess ^. sess_data.sessionUserID) == Just key) $ do
      _sessionStore . ix sessID . sess_data . sessionUserID .= Nothing

-- | Retrieve all users with the 'userIsAdmin' field set to True.
getAdminUsers :: Acid.Query GlobalState [User]
getAdminUsers = filter userIsAdmin . toList <$> view _users

-- | Populate the database with info from the public DB.
importPublicDB :: PublicDB -> Acid.Update GlobalState ()
importPublicDB = put . fromPublicDB

-- | Strip the database from sensitive data and create a 'PublicDB' from it.
exportPublicDB :: Acid.Query GlobalState PublicDB
exportPublicDB = toPublicDB <$> ask

makeAcidic ''GlobalState [
  -- queries
  'getGlobalState,
  'getCategories,
  'getCategory, 'getCategoryMaybe,
  'getCategoryByItem,
  'getItem, 'getItemMaybe,
  'getTrait, 'getTraitMaybe,
  -- add
  'addCategory,
  'addItem,
  'addPro, 'addCon,
  -- set
  'setGlobalState,
  'setCategoryTitle, 'setCategoryGroup, 'setCategoryNotes, 'setCategoryStatus,
    'changeCategoryEnabledSections,
  'setItemName, 'setItemLink, 'setItemHackage, 'setItemSummary, 'setItemNotes, 'setItemEcosystem,
  'setTraitContent,
  -- delete
  'deleteCategory,
  'deleteItem,
  'deleteTrait,
  -- edits
  'getEdit, 'getEdits,
  'registerEdit,
  'removePendingEdit, 'removePendingEdits,
  -- other
  'moveItem, 'moveTrait,
  'restoreCategory, 'restoreItem, 'restoreTrait,
  'setDirty, 'unsetDirty,

  -- sessions
  'loadSession, 'storeSession, 'deleteSession, 'getSessions,

  -- users
  'getUser, 'createUser, 'deleteUser, 'deleteAllUsers,
  'loginUser,

  'getAdminUsers,

  -- PublicDB
  'importPublicDB,
  'exportPublicDB
  ]

-- global state
deriving instance Show GetGlobalState
-- category
deriving instance Show GetCategories
deriving instance Show GetCategoryMaybe
deriving instance Show AddCategory
deriving instance Show DeleteCategory
deriving instance Show SetCategoryGroup
deriving instance Show SetCategoryTitle
deriving instance Show SetCategoryNotes
deriving instance Show SetCategoryStatus
deriving instance Show ChangeCategoryEnabledSections
-- items
deriving instance Show GetItemMaybe
deriving instance Show MoveItem
deriving instance Show DeleteItem
deriving instance Show AddItem
deriving instance Show SetItemName
deriving instance Show SetItemNotes
deriving instance Show SetItemLink
deriving instance Show SetItemEcosystem
deriving instance Show SetItemHackage
deriving instance Show SetItemSummary
-- trait
deriving instance Show MoveTrait
deriving instance Show DeleteTrait
deriving instance Show GetTraitMaybe
deriving instance Show SetTraitContent
deriving instance Show AddPro
deriving instance Show AddCon
-- edit
deriving instance Show RegisterEdit

----------------------------------------------------------------------------
-- DB helpers (have to be at the end of the file)
----------------------------------------------------------------------------

-- | A connection to an open acid-state database (allows making
-- queries/updates, creating checkpoints, etc).
type DB = AcidState GlobalState

-- | Open the database, do something with it, then close the database.
--
-- See Note [acid-state] for the explanation of 'openLocalStateFrom',
-- 'createCheckpoint', etc.
withDB
  :: IO ()               -- ^ Action to run after closing the database
  -> (DB -> IO ())       -- ^ Action to run when the database is open
  -> IO ()
withDB afterClose action = do
  let prepare = openLocalStateFrom "state/" emptyState
      finalise db = do
        putStrLn "Creating an acid-state checkpoint and closing acid-state"
        createCheckpointAndClose' db
        afterClose
  bracket prepare finalise action

-- | Like 'createCheckpoint', but doesn't create a checkpoint if there were
-- no changes made.
createCheckpoint' :: MonadIO m => DB -> m ()
createCheckpoint' db = liftIO $ do
  wasDirty <- Acid.update db UnsetDirty
  when wasDirty $ do
    createArchive db
    createCheckpoint db

-- | Like 'createCheckpointAndClose', but doesn't create a checkpoint if
-- there were no changes made.
createCheckpointAndClose' :: MonadIO m => DB -> m ()
createCheckpointAndClose' db = liftIO $ do
  wasDirty <- Acid.update db UnsetDirty
  if wasDirty then do
    createArchive db
    createCheckpointAndClose db
  else do
    closeAcidState db
