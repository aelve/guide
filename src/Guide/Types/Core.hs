{-# LANGUAGE DeriveDataTypeable #-}
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
-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate

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

-- | A trait (pro or con). Traits are stored in items.
data Trait = Trait {
  _traitUid :: Uid Trait,
  _traitContent :: MarkdownInline }
  deriving (Show, Generic, Data)

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

-- | Kind of an item (items can be libraries, tools, etc).
data ItemKind
  = Library (Maybe Text)  -- Hackage name
  | Tool (Maybe Text)     -- Hackage name
  | Other
  deriving (Eq, Show, Generic, Data)

deriveSafeCopySimple 3 'extension ''ItemKind

hackageName :: Traversal' ItemKind (Maybe Text)
hackageName f (Library x) = Library <$> f x
hackageName f (Tool x)    = Tool <$> f x
hackageName _ Other       = pure Other

instance A.ToJSON ItemKind where
  toJSON (Library x) = A.object [
    "tag"         A..= ("Library" :: Text),
    "contents" A..= x ]
  toJSON (Tool x) = A.object [
    "tag"         A..= ("Tool" :: Text),
    "contents" A..= x ]
  toJSON Other = A.object [
    "tag"         A..= ("Other" :: Text) ]

data ItemKind_v2
  = Library_v2 (Maybe Text)
  | Tool_v2 (Maybe Text)
  | Other_v2

-- TODO: at the next migration change this to deriveSafeCopySimple!
deriveSafeCopy 2 'base ''ItemKind_v2

instance Migrate ItemKind where
  type MigrateFrom ItemKind = ItemKind_v2
  migrate (Library_v2 x) = Library x
  migrate (Tool_v2 x) = Tool x
  migrate Other_v2 = Other

-- | Different kinds of sections inside items. This type is only used for
-- '_categoryEnabledSections'.
data ItemSection
  = ItemProsConsSection
  | ItemEcosystemSection
  | ItemNotesSection
  deriving (Eq, Ord, Show, Generic, Data)

deriveSafeCopySimple 0 'base ''ItemSection

instance A.ToJSON ItemSection where
  toJSON = A.genericToJSON A.defaultOptions

-- TODO: add a field like “people to ask on IRC about this library if you
-- need help”

-- | An item (usually a library). Items are stored in categories.
data Item = Item {
  _itemUid         :: Uid Item,        -- ^ Item ID
  _itemName        :: Text,            -- ^ Item title
  _itemCreated     :: UTCTime,         -- ^ When the item was created
  _itemGroup_      :: Maybe Text,      -- ^ Item group (affects item's color)
  _itemDescription :: MarkdownBlock,   -- ^ Item summary
  _itemPros        :: [Trait],         -- ^ Pros (positive traits)
  _itemProsDeleted :: [Trait],         -- ^ Deleted pros go here (so that
                                       --   it'd be easy to restore them)
  _itemCons        :: [Trait],         -- ^ Cons (negative traits)
  _itemConsDeleted :: [Trait],         -- ^ Deleted cons go here
  _itemEcosystem   :: MarkdownBlock,   -- ^ The ecosystem section
  _itemNotes       :: MarkdownTree,    -- ^ The notes section
  _itemLink        :: Maybe Url,       -- ^ Link to homepage or something
  _itemKind        :: ItemKind         -- ^ Is it a library, tool, etc
  }
  deriving (Show, Generic, Data)

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

-- | Category status
data CategoryStatus
  = CategoryStub                -- ^ “Stub” = just created
  | CategoryWIP                 -- ^ “WIP” = work in progress
  | CategoryFinished            -- ^ “Finished” = complete or nearly complete
  deriving (Eq, Show, Generic, Data)

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

-- | A category
data Category = Category {
  _categoryUid          :: Uid Category,
  _categoryTitle        :: Text,
  -- | When the category was created
  _categoryCreated      :: UTCTime,
  -- | The “grandcategory” of the category (“meta”, “basics”, etc)
  _categoryGroup_       :: Text,
  _categoryStatus       :: CategoryStatus,
  _categoryNotes        :: MarkdownBlock,
  -- | Items stored in the category
  _categoryItems        :: [Item],
  -- | Items that were deleted from the category. We keep them here to make
  -- it easier to restore them
  _categoryItemsDeleted :: [Item],
  -- | Enabled sections in this category. E.g, if this set contains
  -- 'ItemNotesSection', then notes will be shown for each item
  _categoryEnabledSections :: Set ItemSection,
  -- | All groups of items belonging to the category, as well as their
  -- colors. Storing colors explicitly lets us keep colors consistent when
  -- all items in a group are deleted
  _categoryGroups :: Map Text Hue
  }
  deriving (Show, Generic, Data)

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

-- | Category identifier (used in URLs). E.g. for a category with title
-- “Performance optimization” and UID “t3c9hwzo” the slug would be
-- @performance-optimization-t3c9hwzo@.
categorySlug :: Category -> Text
categorySlug category =
  format "{}-{}" (makeSlug (category^.title)) (category^.uid)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | A useful predicate; @hasUid x@ compares given object's UID with @x@.
hasUid :: HasUid a (Uid u) => Uid u -> a -> Bool
hasUid u x = x^.uid == u
