{-# LANGUAGE
TemplateHaskell,
MultiParamTypeClasses,
FunctionalDependencies,
FlexibleInstances,
RecordWildCards,
TypeFamilies,
OverloadedStrings,
GeneralizedNewtypeDeriving,
RankNTypes,
DeriveDataTypeable,
NoImplicitPrelude
  #-}


module Types
(
  Uid(..),
    randomUid,
  Trait(..),
  ItemKind(..),
    hackageName,
  Item(..),
    group_,
    pros,
    cons,
    link,
    kind,
  Hue(..),
    hueToDarkColor,
    hueToLightColor,
  Category(..),
    title,
    groups,
    items,
  GlobalState(..),
    categories,

  -- * Overloaded lenses
  uid,
  content,
  name,
  description,
  notes,

  -- * acid-state methods
  -- ** query
  GetGlobalState(..),
  GetCategories(..),
  GetCategory(..),
  GetCategoryByItem(..),
  GetItem(..),
  GetTrait(..),

  -- ** add
  AddCategory(..),
  AddItem(..),
  AddPro(..),
  AddCon(..),

  -- ** set
  -- *** 'Category'
  SetCategoryTitle(..),
  SetCategoryNotes(..),
  -- *** 'Item'
  SetItemName(..),
  SetItemLink(..),
  SetItemGroup(..),
  SetItemKind(..),
  SetItemDescription(..),
  SetItemNotes(..),
  -- *** 'Trait'
  SetTraitContent(..),

  -- ** delete
  DeleteItem(..),
  DeleteTrait(..),

  -- ** other
  MoveItem(..),
  MoveTrait(..),
  )
where


-- General
import BasePrelude hiding (Category)
-- Lenses
import Lens.Micro.Platform
-- Containers
import qualified Data.Map as M
import Data.Map (Map)
-- Text
import Data.Text (Text)
-- acid-state
import Data.SafeCopy hiding (kind)
import Data.Acid as Acid

-- Local
import Utils


data Trait = Trait {
  _traitUid :: Uid,
  _traitContent :: Text }
  deriving (Eq, Data)

deriveSafeCopy 0 'base ''Trait
makeFields ''Trait

--

data ItemKind
  = Library {_itemKindHackageName :: Maybe Text}
  | Tool {_itemKindHackageName :: Maybe Text}
  | Other
  deriving (Eq, Show, Data)

deriveSafeCopy 2 'base ''ItemKind
makeFields ''ItemKind

data ItemKind_v1
  = Library_v1 {_itemKindOnHackage_v1 :: Bool}
  | Tool_v1 {_itemKindOnHackage_v1 :: Bool}
  | Other_v1

deriveSafeCopy 1 'base ''ItemKind_v1

--

-- TODO: add a field like “people to ask on IRC about this library if you
-- need help”
data Item = Item {
  _itemUid         :: Uid,
  _itemName        :: Text,
  _itemGroup_      :: Maybe Text,
  _itemDescription :: Text,
  _itemPros        :: [Trait],
  _itemCons        :: [Trait],
  _itemNotes       :: Text,
  _itemLink        :: Maybe Url,
  _itemKind        :: ItemKind }
  deriving (Eq, Data)

-- TODO: make a 'Markdown' type alias?

deriveSafeCopy 2 'extension ''Item
makeFields ''Item

-- Old version, needed for safe migration, can be deleted
data Item_v1 = Item_v1 {
  _itemUid_v1         :: Uid,
  _itemName_v1        :: Text,
  _itemGroup__v1      :: Maybe Text,
  _itemDescription_v1 :: Text,
  _itemPros_v1        :: [Trait],
  _itemCons_v1        :: [Trait],
  _itemNotes_v1       :: Text,
  _itemLink_v1        :: Maybe Url,
  _itemKind_v1        :: ItemKind_v1 }

deriveSafeCopy 1 'base ''Item_v1

instance Migrate Item where
  type MigrateFrom Item = Item_v1
  migrate Item_v1{..} = Item {
    _itemUid = _itemUid_v1,
    _itemName = _itemName_v1,
    _itemGroup_ = _itemGroup__v1,
    _itemDescription = _itemDescription_v1,
    _itemPros = _itemPros_v1,
    _itemCons = _itemCons_v1,
    _itemNotes = _itemNotes_v1,
    _itemLink = _itemLink_v1,
    _itemKind = case _itemKind_v1 of
        Library_v1 x -> Library (guard x $> _itemName_v1)
        Tool_v1    x -> Tool    (guard x $> _itemName_v1)
        Other_v1     -> Other }

--

data Hue = NoHue | Hue Int
  deriving (Eq, Ord, Data)

deriveSafeCopy 0 'base ''Hue

instance Show Hue where
  show NoHue   = "0"
  show (Hue n) = show n

{-
https://www.google.com/design/spec/style/color.html#color-color-palette

                50     100    200
              ------ ------ ------
red         : FFEBEE FFCDD2 EF9A9A
pink        : FCE4EC F8BBD0 F48FB1
purple      : F3E5F5 E1BEE7 CE93D8
deep purple : EDE7F6 D1C4E9 B39DDB
indigo      : E8EAF6 C5CAE9 9FA8DA
blue        : E3F2FD BBDEFB 90CAF9
light blue  : E1F5FE B3E5FC 81D4FA
cyan        : E0F7FA B2EBF2 80DEEA
teal        : E0F2F1 B2DFDB 80CBC4
green       : E8F5E9 C8E6C9 A5D6A7
light green : F1F8E9 DCEDC8 C5E1A5
lime        : F9FBE7 F0F4C3 E6EE9C
yellow      : FFFDE7 FFF9C4 FFF59D
amber       : FFF8E1 FFECB3 FFE082
orange      : FFF3E0 FFE0B2 FFCC80
deep orange : FBE9E7 FFCCBC FFAB91
brown       : EFEBE9 D7CCC8 BCAAA4
gray        : FAFAFA F5F5F5 EEEEEE
blue gray   : ECEFF1 CFD8DC B0BEC5
-}

-- TODO: more colors and don't repeat them!
-- TODO: what about colorblind people?
-- TODO: check how all colors look (not just deep purple)
hueToDarkColor :: Hue -> Text
hueToDarkColor NoHue = "#D6D6D6"  -- the color for gray isn't from Google's
                                  -- palette, since their “100” is too light
hueToDarkColor (Hue i) = table !! ((i-1) `mod` length table)
  where
    -- the “100” colors
    table = ["#D1C4E9",   -- deep purple
             "#C8E6C9",   -- green
             "#FFECB3",   -- amber
             "#FFCDD2"]   -- red

hueToLightColor :: Hue -> Text
hueToLightColor NoHue = "#F0F0F0"  -- the color for gray isn't from Google's
                                   -- palette, since their “50” is too light
hueToLightColor (Hue i) = table !! ((i-1) `mod` length table)
  where
    -- the “50” colors
    table = ["#EDE7F6",
             "#E8F5E9",
             "#FFF8E1",
             "#FFEBEE"]

--

data Category = Category {
  _categoryUid :: Uid,
  _categoryTitle :: Text,
  _categoryNotes :: Text,
  _categoryGroups :: Map Text Hue,
  _categoryItems :: [Item] }
  deriving (Eq, Data)

deriveSafeCopy 0 'base ''Category
makeFields ''Category

--

data GlobalState = GlobalState {
  _categories :: [Category] }
  deriving (Data)

deriveSafeCopy 0 'base ''GlobalState
makeLenses ''GlobalState

addGroupIfDoesNotExist :: Text -> Map Text Hue -> Map Text Hue
addGroupIfDoesNotExist g gs
  | M.member g gs = gs
  | otherwise     = M.insert g firstNotTaken gs
  where
    firstNotTaken = head $ map Hue [1..] \\ M.elems gs

traitById :: Uid -> Lens' Item Trait
traitById uid' = singular $
  (pros.each . filtered ((== uid') . view uid)) `failing`
  (cons.each . filtered ((== uid') . view uid))

categoryById :: Uid -> Lens' GlobalState Category
categoryById catId = singular $
  categories.each . filtered ((== catId) . view uid)

categoryByItem :: Uid -> Lens' GlobalState Category
categoryByItem itemId = singular $
  categories.each . filtered hasItem
  where
    hasItem category = itemId `elem` (category^..items.each.uid)

itemById :: Uid -> Lens' GlobalState Item
itemById itemId = singular $
  categories.each . items.each . filtered ((== itemId) . view uid)

-- get

getGlobalState :: Acid.Query GlobalState GlobalState
getGlobalState = view id

getCategories :: Acid.Query GlobalState [Category]
getCategories = view categories

getCategory :: Uid -> Acid.Query GlobalState Category
getCategory uid' = view (categoryById uid')

getCategoryByItem :: Uid -> Acid.Query GlobalState Category
getCategoryByItem uid' = view (categoryByItem uid')

getItem :: Uid -> Acid.Query GlobalState Item
getItem uid' = view (itemById uid')

-- TODO: this doesn't need the item id, but then we have to be a bit cleverer
-- and store a (TraitId -> ItemId) map in global state (and update it
-- accordingly whenever anything happens, so perhaps let's not do it!)
getTrait
  :: Uid   -- ^ Item id
  -> Uid   -- ^ Trait id
  -> Acid.Query GlobalState Trait
getTrait itemId traitId = view (itemById itemId . traitById traitId)

-- add

addCategory
  :: Uid        -- ^ New category's id
  -> Text       -- ^ Title
  -> Acid.Update GlobalState Category
addCategory catId title' = do
  let newCategory = Category {
        _categoryUid = catId,
        _categoryTitle = title',
        _categoryNotes = "",
        _categoryGroups = mempty,
        _categoryItems = [] }
  categories %= (newCategory :)
  return newCategory

addItem
  :: Uid        -- ^ Category id
  -> Uid        -- ^ New item's id
  -> Text       -- ^ Title
  -> ItemKind   -- ^ Kind
  -> Acid.Update GlobalState Item
addItem catId itemId name' kind' = do
  let newItem = Item {
        _itemUid         = itemId,
        _itemName        = name',
        _itemGroup_      = Nothing,
        _itemDescription = "",
        _itemPros        = [],
        _itemCons        = [],
        _itemNotes       = "",
        _itemLink        = Nothing,
        _itemKind        = kind' }
  categoryById catId . items %= (++ [newItem])
  return newItem

addPro
  :: Uid       -- ^ Item id
  -> Uid       -- ^ Trait id
  -> Text
  -> Acid.Update GlobalState Trait
addPro itemId traitId text' = do
  let newTrait = Trait traitId text'
  itemById itemId . pros %= (++ [newTrait])
  return newTrait

addCon
  :: Uid       -- ^ Item id
  -> Uid       -- ^ Trait id
  -> Text
  -> Acid.Update GlobalState Trait
addCon itemId traitId text' = do
  let newTrait = Trait traitId text'
  itemById itemId . cons %= (++ [newTrait])
  return newTrait

-- set

setCategoryTitle :: Uid -> Text -> Acid.Update GlobalState Category
setCategoryTitle catId title' = do
  categoryById catId . title .= title'
  use (categoryById catId)

setCategoryNotes :: Uid -> Text -> Acid.Update GlobalState Category
setCategoryNotes catId notes' = do
  categoryById catId . notes .= notes'
  use (categoryById catId)

setItemName :: Uid -> Text -> Acid.Update GlobalState Item
setItemName itemId name' = do
  itemById itemId . name .= name'
  use (itemById itemId)

setItemLink :: Uid -> Maybe Url -> Acid.Update GlobalState Item
setItemLink itemId link' = do
  itemById itemId . link .= link'
  use (itemById itemId)

-- Also updates the list of groups in the category
setItemGroup :: Uid -> Maybe Text -> Acid.Update GlobalState Item
setItemGroup itemId newGroup = do
  let categoryLens :: Lens' GlobalState Category
      categoryLens = categoryByItem itemId
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
  use itemLens

setItemKind :: Uid -> ItemKind -> Acid.Update GlobalState Item
setItemKind itemId kind' = do
  itemById itemId . kind .= kind'
  use (itemById itemId)

setItemDescription :: Uid -> Text -> Acid.Update GlobalState Item
setItemDescription itemId description' = do
  itemById itemId . description .= description'
  use (itemById itemId)

setItemNotes :: Uid -> Text -> Acid.Update GlobalState Item
setItemNotes itemId notes' = do
  itemById itemId . notes .= notes'
  use (itemById itemId)

setTraitContent :: Uid -> Uid -> Text -> Acid.Update GlobalState Trait
setTraitContent itemId traitId content' = do
  itemById itemId . traitById traitId . content .= content'
  use (itemById itemId . traitById traitId)

-- delete

deleteItem :: Uid -> Acid.Update GlobalState ()
deleteItem itemId = do
  let categoryLens :: Lens' GlobalState Category
      categoryLens = categoryByItem itemId
  let itemLens :: Lens' GlobalState Item
      itemLens = itemById itemId
  -- If the item was the only item in its group, delete the group (and
  -- make the hue available for new items)
  oldGroup <- use (itemLens.group_)
  case oldGroup of
    Nothing -> return ()
    Just g  -> do
      allItems <- use (categoryLens.items)
      let inOurGroup item = item^.group_ == Just g
      when (length (filter inOurGroup allItems) == 1) $
        categoryLens.groups %= M.delete g
  -- And now delete the item
  categoryLens.items %= deleteFirst ((== itemId) . view uid)

deleteTrait :: Uid -> Uid -> Acid.Update GlobalState ()
deleteTrait itemId traitId = do
  itemById itemId . pros %= deleteFirst ((== traitId) . view uid)
  itemById itemId . cons %= deleteFirst ((== traitId) . view uid)

-- other methods

moveItem
  :: Uid
  -> Bool    -- ^ 'True' means up, 'False' means down
  -> Acid.Update GlobalState ()
moveItem itemId up = do
  let move = if up then moveUp else moveDown
  categoryByItem itemId . items %= move ((== itemId) . view uid)

moveTrait
  :: Uid
  -> Uid
  -> Bool    -- ^ 'True' means up, 'False' means down
  -> Acid.Update GlobalState ()
moveTrait itemId traitId up = do
  let move = if up then moveUp else moveDown
  -- The trait is only going to be present in one of the lists so let's do it
  -- in each list because we're too lazy to figure out whether it's a pro or
  -- a con
  itemById itemId . pros %= move ((== traitId) . view uid)
  itemById itemId . cons %= move ((== traitId) . view uid)

-- TODO: add a way to delete a category

makeAcidic ''GlobalState [
  -- queries
  'getGlobalState,
  'getCategories,
  'getCategory,
  'getCategoryByItem,
  'getItem,
  'getTrait,
  -- add
  'addCategory,
  'addItem,
  'addPro, 'addCon,
  -- set
  'setCategoryTitle, 'setCategoryNotes,
  'setItemName, 'setItemLink, 'setItemGroup, 'setItemKind,
    'setItemDescription, 'setItemNotes,
  'setTraitContent,
  -- delete
  'deleteItem,
  'deleteTrait,
  -- other
  'moveItem, 'moveTrait
  ]
