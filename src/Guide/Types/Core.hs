{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


{- |
Core types for content.

The whole site is a list of categories ('Category'). Categories have items
('Item') in them. Items have some sections (fields inside of 'Item'), as well
as traits ('Trait').

It is recommended to use lenses to access fields of various types. All those
lenses are exported from this module. Some lenses (like 'uid' and 'name') are
overloaded and can be used with many types.
-}
module Guide.Types.Core
(
  Trait(..),
  ItemKind(..),
    hackageName,
  ItemSection(..),
  Item(..),
    pros,
    prosDeleted,
    cons,
    consDeleted,
    ecosystem,
    link,
    kind,
  Hue(..),
    hueToDarkColor,
    hueToLightColor,
  CategoryStatus(..),
  Category(..),
    title,
    status,
    enabledSections,
    groups,
    items,
    itemsDeleted,
    categorySlug,

  -- * Overloaded things
  uid,
  hasUid,
  content,
  name,
  description,
  notes,
  created,
  group_,
)
where


import Imports

-- Text
import qualified Data.Text.All as T
-- Containers
import qualified Data.Set as S
-- JSON
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
-- acid-state
import Data.SafeCopy hiding (kind)

import Guide.SafeCopy
import Guide.Markdown
import Guide.Utils
import Guide.Types.Hue


----------------------------------------------------------------------------
-- General notes on code
----------------------------------------------------------------------------

{-

If you want to add a field to one of the types, see Note [extending types].

For an explanation of deriveSafeCopySorted, see Note [acid-state].

-}

----------------------------------------------------------------------------
-- Trait
----------------------------------------------------------------------------

data Trait = Trait {
  _traitUid :: Uid Trait,
  _traitContent :: MarkdownInline }
  deriving (Show, Generic)

deriveSafeCopySorted 4 'extension ''Trait
makeFields ''Trait

changelog ''Trait (Current 4, Past 3) []
deriveSafeCopySorted 3 'base ''Trait_v3

instance A.ToJSON Trait where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = over _head toLower . drop (T.length "_trait") }

----------------------------------------------------------------------------
-- Item
----------------------------------------------------------------------------

data ItemKind
  = Library {_itemKindHackageName :: Maybe Text}
  | Tool {_itemKindHackageName :: Maybe Text}
  | Other
  deriving (Eq, Show, Generic)

deriveSafeCopySimple 3 'extension ''ItemKind
makeFields ''ItemKind

instance A.ToJSON ItemKind where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = over _head toLower . drop (T.length "_itemKind") }

data ItemKind_v2
  = Library_v2 {_itemKindHackageName_v2 :: Maybe Text}
  | Tool_v2 {_itemKindHackageName_v2 :: Maybe Text}
  | Other_v2

-- TODO: at the next migration change this to deriveSafeCopySimple!
deriveSafeCopy 2 'base ''ItemKind_v2

instance Migrate ItemKind where
  type MigrateFrom ItemKind = ItemKind_v2
  migrate Library_v2{..} = Library {
    _itemKindHackageName = _itemKindHackageName_v2 }
  migrate Tool_v2{..} = Tool {
    _itemKindHackageName = _itemKindHackageName_v2 }
  migrate Other_v2 = Other

-- | Different kinds of sections inside items. This type is only used for
-- '_categoryEnabledSections'.
data ItemSection
  = ItemProsConsSection
  | ItemEcosystemSection
  | ItemNotesSection
  deriving (Eq, Ord, Show, Generic)

deriveSafeCopySimple 0 'base ''ItemSection

instance A.ToJSON ItemSection where
  toJSON = A.genericToJSON A.defaultOptions

-- TODO: add a field like “people to ask on IRC about this library if you
-- need help”

data Item = Item {
  _itemUid         :: Uid Item,
  _itemName        :: Text,
  _itemCreated     :: UTCTime,
  _itemGroup_      :: Maybe Text,
  _itemDescription :: MarkdownBlock,
  _itemPros        :: [Trait],
  _itemProsDeleted :: [Trait],
  _itemCons        :: [Trait],
  _itemConsDeleted :: [Trait],
  _itemEcosystem   :: MarkdownBlock,
  _itemNotes       :: MarkdownBlockWithTOC,
  _itemLink        :: Maybe Url,
  _itemKind        :: ItemKind }
  deriving (Show, Generic)

deriveSafeCopySorted 11 'extension ''Item
makeFields ''Item

changelog ''Item (Current 11, Past 10) []
deriveSafeCopySorted 10 'base ''Item_v10

instance A.ToJSON Item where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = over _head toLower . drop (T.length "_item") }

----------------------------------------------------------------------------
-- Category
----------------------------------------------------------------------------

data CategoryStatus
  = CategoryStub
  | CategoryWIP
  | CategoryFinished
  deriving (Eq, Show, Generic)

deriveSafeCopySimple 2 'extension ''CategoryStatus

instance A.ToJSON CategoryStatus where
  toJSON = A.genericToJSON A.defaultOptions

data CategoryStatus_v1
  = CategoryStub_v1
  | CategoryWIP_v1
  | CategoryMostlyDone_v1
  | CategoryFinished_v1

deriveSafeCopySimple 1 'base ''CategoryStatus_v1

instance Migrate CategoryStatus where
  type MigrateFrom CategoryStatus = CategoryStatus_v1
  migrate CategoryStub_v1 = CategoryStub
  migrate CategoryWIP_v1 = CategoryWIP
  migrate CategoryMostlyDone_v1 = CategoryFinished
  migrate CategoryFinished_v1 = CategoryFinished

data Category = Category {
  _categoryUid :: Uid Category,
  _categoryTitle :: Text,
  -- | The “grandcategory” of the category (“meta”, “basics”, “specialised
  -- needs”, etc)
  _categoryGroup_ :: Text,
  -- | Enabled sections in this category. For instance, if this set contains
  -- 'ItemNotesSection', then notes will be shown for each item.
  _categoryEnabledSections :: Set ItemSection,
  _categoryCreated :: UTCTime,
  _categoryStatus :: CategoryStatus,
  _categoryNotes :: MarkdownBlock,
  -- | All groups of items belonging to the category, as well as their
  -- colors. We could assign colors to items when we render the category
  -- (something like “if haven't seen this group yet, assign a new color to
  -- it and render it with this color”, but this way is easier and also
  -- allows us to keep the colors of all other groups the same when one item
  -- has been deleted.
  _categoryGroups :: Map Text Hue,
  _categoryItems :: [Item],
  _categoryItemsDeleted :: [Item] }
  deriving (Show, Generic)

deriveSafeCopySorted 11 'extension ''Category
makeFields ''Category

changelog ''Category (Current 11, Past 10)
  [Removed "_categoryProsConsEnabled"  [t|Bool|],
   Removed "_categoryEcosystemEnabled" [t|Bool|],
   Removed "_categoryNotesEnabled"     [t|Bool|],
   Added   "_categoryEnabledSections"  [hs|
     S.fromList $ concat
       [ [ItemProsConsSection  | _categoryProsConsEnabled]
       , [ItemEcosystemSection | _categoryEcosystemEnabled]
       , [ItemNotesSection     | _categoryNotesEnabled] ] |] ]
deriveSafeCopySorted 10 'extension ''Category_v10

changelog ''Category (Past 10, Past 9)
  [Added "_categoryNotesEnabled" [hs|True|]]
deriveSafeCopySorted 9 'extension ''Category_v9

changelog ''Category (Past 9, Past 8) []
deriveSafeCopySorted 8 'base ''Category_v8

instance A.ToJSON Category where
  toJSON = A.genericToJSON A.defaultOptions {
    A.fieldLabelModifier = over _head toLower . drop (T.length "_category") }

categorySlug :: Category -> Text
categorySlug category =
  T.format "{}-{}" (makeSlug (category^.title), category^.uid)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

hasUid :: HasUid a (Uid u) => Uid u -> a -> Bool
hasUid u x = x^.uid == u
