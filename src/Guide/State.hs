{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


{- |
Site's database, and methods for manipulating it.
-}
module Guide.State
(
  GlobalState(..),
    categories,
    categoriesDeleted,
    actions,
    pendingEdits,
    editIdCounter,
    findCategoryByItem,

  -- * acid-state methods
  -- ** query
  GetGlobalState(..),
  GetCategories(..),
  GetCategory(..), GetCategoryMaybe(..),
  GetCategoryByItem(..),
  GetItem(..),
  GetTrait(..),

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
  SetItemGroup(..),
  SetItemKind(..),
  SetItemDescription(..),
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

  -- ** actions
  RegisterAction(..),

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

  AddCreds(..), RemoveCreds(..),
  LoginUserCreds(..),

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

-- Containers
import qualified Data.Map as M
import qualified Data.Set as S
-- Text
import qualified Data.Text.All as T
-- Network
import Data.IP
-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate
import Data.Acid as Acid
--
import Web.Spock.Internal.SessionManager (SessionId)

import Guide.Utils
import Guide.Markdown
import Guide.Types.Core
import Guide.Types.Edit
import Guide.Types.Action
import Guide.Types.Session
import Guide.Types.User
import Guide.Types.Creds


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


Cache.hs
~~~~~~~~~~~~~~~~~~~~~~~~~

  1. If the field is non-trivial (e.g. “notes”) and it makes sense to cache
     it, add it to 'CacheKey'.

  2. Update 'cacheDepends'.


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

  1. Add a case to 'invalidateCacheForEdit'.

  2. Add a case to 'undoEdit'.

  3. If the field is user-editable, add a method for changing it to
     'setMethods'.

-}

data GlobalState = GlobalState {
  _categories :: [Category],
  _categoriesDeleted :: [Category],
  _actions :: [(Action, ActionDetails)],
  -- | Pending edits, newest first
  _pendingEdits :: [(Edit, EditDetails)],
  -- | ID of next edit that will be made
  _editIdCounter :: Int,
  -- | Sessions
  _sessionStore :: Map SessionId GuideSession,
  -- | Users
  _users :: Map (Uid User) User,
  -- | External credentials
  _creds :: Map (Uid User) [Creds],
  -- | The dirty bit (needed to choose whether to make a checkpoint or not)
  _dirty :: Bool }
  deriving (Show)

deriveSafeCopySorted 9 'extension ''GlobalState
makeLenses ''GlobalState

changelog ''GlobalState (Current 9, Past 8) [
  Added "_creds" [hs|M.empty|]
  ]
deriveSafeCopySorted 8 'extension ''GlobalState_v8

changelog ''GlobalState (Past 8, Past 7) [
  Added "_sessionStore" [hs|M.empty|],
  Added "_users" [hs|M.empty|]
  ]
deriveSafeCopySorted 7 'base ''GlobalState_v7

addGroupIfDoesNotExist :: Text -> Map Text Hue -> Map Text Hue
addGroupIfDoesNotExist g gs
  | M.member g gs = gs
  | otherwise     = M.insert g firstNotTaken gs
  where
    firstNotTaken = head $ map Hue [1..] \\ M.elems gs

traitById :: Uid Trait -> Lens' Item Trait
traitById uid' = singular $
  (pros.each . filtered (hasUid uid')) `failing`
  (cons.each . filtered (hasUid uid')) `failing`
  error ("traitById: couldn't find trait with uid " ++
         T.unpack (uidToText uid'))

categoryById :: Uid Category -> Lens' GlobalState Category
categoryById catId = singular $
  categories.each . filtered (hasUid catId) `failing`
  error ("categoryById: couldn't find category with uid " ++
         T.unpack (uidToText catId))

itemById :: Uid Item -> Lens' GlobalState Item
itemById itemId = singular $
  categories.each . items.each . filtered (hasUid itemId) `failing`
  error ("itemById: couldn't find item with uid " ++
         T.unpack (uidToText itemId))

findCategoryByItem :: Uid Item -> GlobalState -> Category
findCategoryByItem itemId s =
  fromMaybe (error err) (find hasItem (s^.categories))
  where
    err = "findCategoryByItem: couldn't find category with item with uid " ++
          T.unpack (uidToText itemId)
    hasItem category = itemId `elem` (category^..items.each.uid)

-- | 'PublicDB' contains all safe data from 'GlobalState'.
-- Difference from 'GlobalState':
-- * 'User' replaced with 'PublicUser'
-- * Sessions information removed
-- * Dirty flag removed
data PublicDB = PublicDB {
  publicCategories        :: [Category],
  publicCategoriesDeleted :: [Category],
  publicActions           :: [(Action, ActionDetails)],
  publicPendingEdits      :: [(Edit, EditDetails)],
  publicEditIdCounter     :: Int,
  publicUsers             :: Map (Uid User) PublicUser,
  publicCreds             :: Map (Uid User) [PublicCreds]}
  deriving (Show)

-- NOTE: you don't need to write migrations for 'PublicDB' but you still
-- need to increase the version when the type changes, so that old clients
-- wouldn't get cryptic error messages like “not enough bytes” when trying
-- to deserialize a new version of 'PublicDB' that they can't handle.
deriveSafeCopySorted 1 'base ''PublicDB

-- | Converts 'GlobalState' to 'PublicDB' type stripping private data.
toPublicDB :: GlobalState -> PublicDB
toPublicDB GlobalState{..} =
  PublicDB {
    publicCategories        = _categories,
    publicCategoriesDeleted = _categoriesDeleted,
    publicActions           = _actions,
    publicPendingEdits      = _pendingEdits,
    publicEditIdCounter     = _editIdCounter,
    publicUsers             = fmap userToPublic _users,
    publicCreds             = (fmap . fmap) credsToPublic _creds
  }

-- | Converts 'PublicDB' to 'GlobalState' type filling in non-existing data with
-- default values.
fromPublicDB :: PublicDB -> GlobalState
fromPublicDB PublicDB{..} =
  GlobalState {
    _categories        = publicCategories,
    _categoriesDeleted = publicCategoriesDeleted,
    _actions           = publicActions,
    _pendingEdits      = publicPendingEdits,
    _editIdCounter     = publicEditIdCounter,
    _sessionStore      = M.empty,
    _users             = fmap publicUserToUser publicUsers,
    _creds             = (fmap . fmap) publicCredsToCreds publicCreds,
    _dirty             = True
  }

-- get

getGlobalState :: Acid.Query GlobalState GlobalState
getGlobalState = view id

getCategories :: Acid.Query GlobalState [Category]
getCategories = view categories

getCategory :: Uid Category -> Acid.Query GlobalState Category
getCategory uid' = view (categoryById uid')

getCategoryMaybe :: Uid Category -> Acid.Query GlobalState (Maybe Category)
getCategoryMaybe uid' = preview (categoryById uid')

getCategoryByItem :: Uid Item -> Acid.Query GlobalState Category
getCategoryByItem uid' = findCategoryByItem uid' <$> ask

getItem :: Uid Item -> Acid.Query GlobalState Item
getItem uid' = view (itemById uid')

-- TODO: this doesn't need the item id, but then we have to be a bit cleverer
-- and store a (TraitId -> ItemId) map in global state (and update it
-- accordingly whenever anything happens, so perhaps let's not do it!)
getTrait :: Uid Item -> Uid Trait -> Acid.Query GlobalState Trait
getTrait itemId traitId = view (itemById itemId . traitById traitId)

-- add

addCategory
  :: Uid Category    -- ^ New category's id
  -> Text            -- ^ Title
  -> UTCTime         -- ^ Creation time
  -> Acid.Update GlobalState (Edit, Category)
addCategory catId title' created' = do
  let newCategory = Category {
        _categoryUid = catId,
        _categoryTitle = title',
        _categoryGroup_ = "Miscellaneous",
        _categoryEnabledSections = S.fromList [
            ItemProsConsSection,
            ItemEcosystemSection,
            ItemNotesSection ],
        _categoryCreated = created',
        _categoryStatus = CategoryStub,
        _categoryNotes = toMarkdownBlock "",
        _categoryGroups = mempty,
        _categoryItems = [],
        _categoryItemsDeleted = [] }
  categories %= (newCategory :)
  let edit = Edit'AddCategory catId title'
  return (edit, newCategory)

addItem
  :: Uid Category    -- ^ Category id
  -> Uid Item        -- ^ New item's id
  -> Text            -- ^ Name
  -> UTCTime         -- ^ Creation time
  -> ItemKind        -- ^ Kind
  -> Acid.Update GlobalState (Edit, Item)
addItem catId itemId name' created' kind' = do
  let newItem = Item {
        _itemUid         = itemId,
        _itemName        = name',
        _itemCreated     = created',
        _itemGroup_      = Nothing,
        _itemDescription = toMarkdownBlock "",
        _itemPros        = [],
        _itemProsDeleted = [],
        _itemCons        = [],
        _itemConsDeleted = [],
        _itemEcosystem   = toMarkdownBlock "",
        _itemNotes       = let pref = "item-notes-" <> uidToText itemId <> "-"
                           in  toMarkdownTree pref "",
        _itemLink        = Nothing,
        _itemKind        = kind' }
  categoryById catId . items %= (++ [newItem])
  let edit = Edit'AddItem catId itemId name'
  return (edit, newItem)

addPro
  :: Uid Item       -- ^ Item id
  -> Uid Trait      -- ^ New trait's id
  -> Text
  -> Acid.Update GlobalState (Edit, Trait)
addPro itemId traitId text' = do
  let newTrait = Trait traitId (toMarkdownInline text')
  itemById itemId . pros %= (++ [newTrait])
  let edit = Edit'AddPro itemId traitId text'
  return (edit, newTrait)

addCon
  :: Uid Item       -- ^ Item id
  -> Uid Trait      -- ^ New trait's id
  -> Text
  -> Acid.Update GlobalState (Edit, Trait)
addCon itemId traitId text' = do
  let newTrait = Trait traitId (toMarkdownInline text')
  itemById itemId . cons %= (++ [newTrait])
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
  oldTitle <- categoryById catId . title <<.= title'
  let edit = Edit'SetCategoryTitle catId oldTitle title'
  (edit,) <$> use (categoryById catId)

setCategoryGroup :: Uid Category -> Text -> Acid.Update GlobalState (Edit, Category)
setCategoryGroup catId group' = do
  oldGroup <- categoryById catId . group_ <<.= group'
  let edit = Edit'SetCategoryGroup catId oldGroup group'
  (edit,) <$> use (categoryById catId)

setCategoryNotes :: Uid Category -> Text -> Acid.Update GlobalState (Edit, Category)
setCategoryNotes catId notes' = do
  oldNotes <- categoryById catId . notes <<.= toMarkdownBlock notes'
  let edit = Edit'SetCategoryNotes catId (oldNotes ^. mdText) notes'
  (edit,) <$> use (categoryById catId)

setCategoryStatus :: Uid Category -> CategoryStatus -> Acid.Update GlobalState (Edit, Category)
setCategoryStatus catId status' = do
  oldStatus <- categoryById catId . status <<.= status'
  let edit = Edit'SetCategoryStatus catId oldStatus status'
  (edit,) <$> use (categoryById catId)

changeCategoryEnabledSections
  :: Uid Category
  -> Set ItemSection     -- ^ Sections to enable
  -> Set ItemSection     -- ^ Sections to disable
  -> Acid.Update GlobalState (Edit, Category)
changeCategoryEnabledSections catId toEnable toDisable = do
  categoryById catId . enabledSections %= \sections ->
    (sections <> toEnable) S.\\ toDisable
  let edit = Edit'ChangeCategoryEnabledSections catId toEnable toDisable
  (edit,) <$> use (categoryById catId)

setItemName :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemName itemId name' = do
  oldName <- itemById itemId . name <<.= name'
  let edit = Edit'SetItemName itemId oldName name'
  (edit,) <$> use (itemById itemId)

setItemLink :: Uid Item -> Maybe Url -> Acid.Update GlobalState (Edit, Item)
setItemLink itemId link' = do
  oldLink <- itemById itemId . link <<.= link'
  let edit = Edit'SetItemLink itemId oldLink link'
  (edit,) <$> use (itemById itemId)

-- Also updates the list of groups in the category
setItemGroup :: Uid Item -> Maybe Text -> Acid.Update GlobalState (Edit, Item)
setItemGroup itemId newGroup = do
  catId <- view uid . findCategoryByItem itemId <$> get
  let categoryLens :: Lens' GlobalState Category
      categoryLens = categoryById catId
  let itemLens :: Lens' GlobalState Item
      itemLens = itemById itemId
  -- If the group is new, add it to the list of groups in the category (which
  -- causes a new hue to be generated, too)
  case newGroup of
    Nothing -> return ()
    Just x  -> categoryLens.groups %= addGroupIfDoesNotExist x
  -- Update list of groups if the group is going to be empty after the item
  -- is moved to a different group. Note that this is done after adding a new
  -- group because we also want the color to change. So, if the item was the
  -- only item in its group, the sequence of actions is as follows:
  --
  --   * new group is added (and hence a new color is assigned)
  --   * old group is deleted (and now the old color is unused)
  oldGroup <- use (itemLens.group_)
  case oldGroup of
    Nothing -> return ()
    Just g  -> when (oldGroup /= newGroup) $ do
      allItems <- use (categoryLens.items)
      let inOurGroup item = item^.group_ == Just g
      when (length (filter inOurGroup allItems) == 1) $
        categoryLens.groups %= M.delete g
  -- Now we can actually change the group
  itemLens.group_ .= newGroup
  let edit = Edit'SetItemGroup itemId oldGroup newGroup
  (edit,) <$> use itemLens

setItemKind :: Uid Item -> ItemKind -> Acid.Update GlobalState (Edit, Item)
setItemKind itemId kind' = do
  oldKind <- itemById itemId . kind <<.= kind'
  let edit = Edit'SetItemKind itemId oldKind kind'
  (edit,) <$> use (itemById itemId)

setItemDescription :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemDescription itemId description' = do
  oldDescr <- itemById itemId . description <<.=
                toMarkdownBlock description'
  let edit = Edit'SetItemDescription itemId
               (oldDescr ^. mdText) description'
  (edit,) <$> use (itemById itemId)

setItemNotes :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemNotes itemId notes' = do
  let pref = "item-notes-" <> uidToText itemId <> "-"
  oldNotes <- itemById itemId . notes <<.=
                toMarkdownTree pref notes'
  let edit = Edit'SetItemNotes itemId (oldNotes ^. mdText) notes'
  (edit,) <$> use (itemById itemId)

setItemEcosystem :: Uid Item -> Text -> Acid.Update GlobalState (Edit, Item)
setItemEcosystem itemId ecosystem' = do
  oldEcosystem <- itemById itemId . ecosystem <<.=
                    toMarkdownBlock ecosystem'
  let edit = Edit'SetItemEcosystem itemId
               (oldEcosystem ^. mdText) ecosystem'
  (edit,) <$> use (itemById itemId)

setTraitContent :: Uid Item -> Uid Trait -> Text -> Acid.Update GlobalState (Edit, Trait)
setTraitContent itemId traitId content' = do
  oldContent <- itemById itemId . traitById traitId . content <<.=
                  toMarkdownInline content'
  let edit = Edit'SetTraitContent itemId traitId
               (oldContent ^. mdText) content'
  (edit,) <$> use (itemById itemId . traitById traitId)

-- delete

deleteCategory :: Uid Category -> Acid.Update GlobalState (Either String Edit)
deleteCategory catId = do
  mbCategory <- preuse (categoryById catId)
  case mbCategory of
    Nothing       -> return (Left "category not found")
    Just category -> do
      mbCategoryPos <- findIndex (hasUid catId) <$> use categories
      case mbCategoryPos of
        Nothing          -> return (Left "category not found")
        Just categoryPos -> do
          categories %= deleteAt categoryPos
          categoriesDeleted %= (category:)
          return (Right (Edit'DeleteCategory catId categoryPos))

deleteItem :: Uid Item -> Acid.Update GlobalState (Either String Edit)
deleteItem itemId = do
  catId <- view uid . findCategoryByItem itemId <$> get
  let categoryLens :: Lens' GlobalState Category
      categoryLens = categoryById catId
  let itemLens :: Lens' GlobalState Item
      itemLens = itemById itemId
  mbItem <- preuse itemLens
  case mbItem of
    Nothing   -> return (Left "item not found")
    Just item -> do
      allItems <- use (categoryLens.items)
      -- If the item was the only item in its group, delete the group (and
      -- make the hue available for new items)
      case item^.group_ of
        Nothing       -> return ()
        Just oldGroup -> do
          let itemsInOurGroup = [item' | item' <- allItems,
                                         item'^.group_ == Just oldGroup]
          when (length itemsInOurGroup == 1) $
            categoryLens.groups %= M.delete oldGroup
      -- And now delete the item (i.e. move it to “deleted”)
      case findIndex (hasUid itemId) allItems of
        Nothing      -> return (Left "item not found")
        Just itemPos -> do
          categoryLens.items        %= deleteAt itemPos
          categoryLens.itemsDeleted %= (item:)
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
      case (find (hasUid traitId) (item^.pros),
            find (hasUid traitId) (item^.cons)) of
        -- It's in neither group, which means it was deleted. Do nothing.
        (Nothing, Nothing) -> return (Left "trait not found")
        -- It's a pro
        (Just trait, _) -> do
          mbTraitPos <- findIndex (hasUid traitId) <$> use (itemLens.pros)
          case mbTraitPos of
            Nothing       -> return (Left "trait not found")
            Just traitPos -> do
              itemLens.pros        %= deleteAt traitPos
              itemLens.prosDeleted %= (trait:)
              return (Right (Edit'DeleteTrait itemId traitId traitPos))
        -- It's a con
        (_, Just trait) -> do
          mbTraitPos <- findIndex (hasUid traitId) <$> use (itemLens.cons)
          case mbTraitPos of
            Nothing       -> return (Left "trait not found")
            Just traitPos -> do
              itemLens.cons        %= deleteAt traitPos
              itemLens.consDeleted %= (trait:)
              return (Right (Edit'DeleteTrait itemId traitId traitPos))

-- other methods

moveItem
  :: Uid Item
  -> Bool       -- ^ 'True' means up, 'False' means down
  -> Acid.Update GlobalState Edit
moveItem itemId up = do
  let move = if up then moveUp else moveDown
  catId <- view uid . findCategoryByItem itemId <$> get
  categoryById catId . items %= move (hasUid itemId)
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
  itemById itemId . pros %= move (hasUid traitId)
  itemById itemId . cons %= move (hasUid traitId)
  return (Edit'MoveTrait itemId traitId up)

restoreCategory :: Uid Category -> Int -> Acid.Update GlobalState (Either String ())
restoreCategory catId pos = do
  deleted <- use categoriesDeleted
  case find (hasUid catId) deleted of
    Nothing -> return (Left "category not found in deleted categories")
    Just category -> do
      categoriesDeleted %= deleteFirst (hasUid catId)
      categories        %= insertOrAppend pos category
      return (Right ())

restoreItem :: Uid Item -> Int -> Acid.Update GlobalState (Either String ())
restoreItem itemId pos = do
  let ourCategory = any (hasUid itemId) . view itemsDeleted
  allCategories <- use (categories <> categoriesDeleted)
  case find ourCategory allCategories of
    Nothing -> return (Left "item not found in deleted items")
    Just category -> do
      let item = fromJust (find (hasUid itemId) (category^.itemsDeleted))
      let category' = category
            & itemsDeleted %~ deleteFirst (hasUid itemId)
            & items        %~ insertOrAppend pos item
      categories        . each . filtered ourCategory .= category'
      categoriesDeleted . each . filtered ourCategory .= category'
      return (Right ())

restoreTrait :: Uid Item -> Uid Trait -> Int -> Acid.Update GlobalState (Either String ())
restoreTrait itemId traitId pos = do
  let getItems = view (items <> itemsDeleted)
      ourCategory = any (hasUid itemId) . getItems
  allCategories <- use (categories <> categoriesDeleted)
  case find ourCategory allCategories of
    Nothing -> return (Left "item -that the trait belongs to- not found")
    Just category -> do
      let item = fromJust (find (hasUid itemId) (getItems category))
      case (find (hasUid traitId) (item^.prosDeleted),
            find (hasUid traitId) (item^.consDeleted)) of
        (Nothing, Nothing) ->
          return (Left "trait not found in deleted traits")
        (Just trait, _) -> do
          let item' = item
                & prosDeleted %~ deleteFirst (hasUid traitId)
                & pros        %~ insertOrAppend pos trait
          let category' = category
                & items        . each . filtered (hasUid itemId) .~ item'
                & itemsDeleted . each . filtered (hasUid itemId) .~ item'
          categories        . each . filtered ourCategory .= category'
          categoriesDeleted . each . filtered ourCategory .= category'
          return (Right ())
        (_, Just trait) -> do
          let item' = item
                & consDeleted %~ deleteFirst (hasUid traitId)
                & cons        %~ insertOrAppend pos trait
          let category' = category
                & items        . each . filtered (hasUid itemId) .~ item'
                & itemsDeleted . each . filtered (hasUid itemId) .~ item'
          categories        . each . filtered ourCategory .= category'
          categoriesDeleted . each . filtered ourCategory .= category'
          return (Right ())

-- TODO: maybe have a single list of traits with pro/con being signified by
-- something like TraitType? or maybe TraitType could even be a part of the
-- trait itself?

getEdit :: Int -> Acid.Query GlobalState (Edit, EditDetails)
getEdit n = do
  edits <- view pendingEdits
  case find ((== n) . editId . snd) edits of
    Nothing   -> error ("no edit with id " ++ show n)
    Just edit -> return edit

-- | Returns edits in order from latest to earliest.
getEdits
  :: Int            -- ^ Id of latest edit
  -> Int            -- ^ Id of earliest edit
  -> Acid.Query GlobalState [(Edit, EditDetails)]
getEdits m n =
  filter (\(_, d) -> n <= editId d && editId d <= m) <$> view pendingEdits

-- | The edit won't be registered if it's vacuous (see 'isVacuousEdit').
registerEdit
  :: Edit
  -> Maybe IP
  -> UTCTime
  -> Acid.Update GlobalState ()
registerEdit ed ip date = do
  id' <- use editIdCounter
  let details = EditDetails {
        editIP   = ip,
        editDate = date,
        editId   = id' }
  pendingEdits %= ((ed, details):)
  editIdCounter += 1

removePendingEdit :: Int -> Acid.Update GlobalState (Edit, EditDetails)
removePendingEdit n = do
  edits <- use pendingEdits
  case find ((== n) . editId . snd) edits of
    Nothing   -> error ("no edit with id " ++ show n)
    Just edit -> do
      pendingEdits %= deleteFirst ((== n) . editId . snd)
      return edit

removePendingEdits
  :: Int            -- ^ Id of latest edit
  -> Int            -- ^ Id of earliest edit
  -> Acid.Update GlobalState ()
removePendingEdits m n = do
  pendingEdits %= filter (\(_, d) -> editId d < n || m < editId d)

registerAction
  :: Action
  -> Maybe IP
  -> UTCTime
  -> Url                          -- ^ Base URL
  -> Maybe Url                    -- ^ Referrer
  -> Maybe Text                   -- ^ User-agent
  -> Acid.Update GlobalState ()
registerAction act ip date baseUrl ref ua = do
  let details = ActionDetails {
        actionIP        = ip,
        actionDate      = date,
        actionReferrer  = case T.stripPrefix baseUrl <$> ref of
                            Nothing       -> Nothing
                            Just Nothing  -> ExternalReferrer <$> ref
                            Just (Just s) -> Just (InternalReferrer s),
        actionUserAgent = ua }
  actions %= ((act, details) :)

setDirty :: Acid.Update GlobalState ()
setDirty = dirty .= True

unsetDirty :: Acid.Update GlobalState Bool
unsetDirty = dirty <<.= False

-- | Retrieves a session by 'SessionID'.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
loadSession :: SessionId -> Acid.Query GlobalState (Maybe GuideSession)
loadSession key = view (sessionStore . at key)

-- | Stores a session object.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
storeSession :: GuideSession -> Acid.Update GlobalState ()
storeSession sess = do
  sessionStore %= M.insert (sess ^. sess_id) sess
  setDirty

-- | Deletes a session by 'SessionID'.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
deleteSession :: SessionId -> Acid.Update GlobalState ()
deleteSession key = do
  sessionStore %= M.delete key
  setDirty

-- | Retrieves all sessions.
-- Note: This utilizes a "wrapper" around Spock.Session, 'GuideSession'.
getSessions :: Acid.Query GlobalState [GuideSession]
getSessions = do
  m <- view sessionStore
  return . map snd $ M.toList m

-- | Retrieves a user by their unique identifier.
getUser :: Uid User -> Acid.Query GlobalState (Maybe User)
getUser key = view (users . at key)

-- | Creates a user, maintaining unique constraints on certain fields.
createUser :: User -> Acid.Update GlobalState Bool
createUser user = do
  m <- toList <$> use users
  if all (canCreateUser user) (m ^.. each)
  then do
    users %= M.insert (user ^. userID) user
    return True
  else
    return False

-- | Remove a user completely. Unsets all user sessions with this user ID.
deleteUser :: Uid User -> Acid.Update GlobalState ()
deleteUser key = do
  users %= M.delete key
  logoutUserGlobally key
  setDirty

deleteAllUsers :: Acid.Update GlobalState ()
deleteAllUsers = do
  mapM_ logoutUserGlobally . M.keys =<< use users
  users .= mempty
  setDirty

-- | Given an email address and a password, return the user if it exists
-- and the password is correct.
loginUser :: Text -> ByteString -> Acid.Query GlobalState (Either String User)
loginUser email password = do
  matches <- filter (\u -> u ^. userEmail == email) . toList <$> view users
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
  sessions <- use sessionStore
  for_ (M.toList sessions) $ \(sessID, sess) -> do
    when ((sess ^. sess_data.sessionUserID) == Just key) $ do
      sessionStore . ix sessID . sess_data . sessionUserID .= Nothing

-- | Retrieve all users with the 'userIsAdmin' field set to True.
getAdminUsers :: Acid.Query GlobalState [User]
getAdminUsers = filter (^. userIsAdmin) . toList <$> view users

-- | Add external credentials to a user account.
addCreds :: User -> Creds -> Acid.Update GlobalState Bool
addCreds user userCreds = do
  allCreds <- concat . toList <$> use creds
  if canAddCreds userCreds allCreds
  then do
    creds %= M.insertWith (++) (user ^. userID) [userCreds]
    return True
  else
    return False

-- | Remove external credentials to a user account.
removeCreds :: User -> Creds -> Acid.Update GlobalState ()
removeCreds user userCreds = do
  creds %= M.adjust (filter filterCred) (user ^. userID)
  where
    filterCred cred |    cred ^. credsProvider == userCreds ^. credsProvider
                      && cred ^. credsId       == userCreds ^. credsId
                      = False
                    | otherwise = True

-- | Get user from creds
loginUserCreds :: Creds -> Acid.Query GlobalState (Maybe User)
loginUserCreds userCreds = do
  matches <- filter (\(_, cs) -> any matchCred cs) . M.toList <$> view creds
  case matches of
    [(key, _)] -> view (users . at key)
    _ -> return Nothing
  where
    matchCred cred =    cred ^. credsProvider == userCreds ^. credsProvider
                     && cred ^. credsId       == userCreds ^. credsId

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
  'getItem,
  'getTrait,
  -- add
  'addCategory,
  'addItem,
  'addPro, 'addCon,
  -- set
  'setGlobalState,
  'setCategoryTitle, 'setCategoryGroup, 'setCategoryNotes, 'setCategoryStatus,
    'changeCategoryEnabledSections,
  'setItemName, 'setItemLink, 'setItemGroup, 'setItemKind,
    'setItemDescription, 'setItemNotes, 'setItemEcosystem,
  'setTraitContent,
  -- delete
  'deleteCategory,
  'deleteItem,
  'deleteTrait,
  -- edits
  'getEdit, 'getEdits,
  'registerEdit,
  'removePendingEdit, 'removePendingEdits,
  -- actions
  'registerAction,
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

  -- creds
  'addCreds, 'removeCreds,
  'loginUserCreds,

  -- PublicDB
  'importPublicDB,
  'exportPublicDB
  ]
