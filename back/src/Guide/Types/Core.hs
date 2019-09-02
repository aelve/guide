{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Core types for content.
--
-- The whole site is a list of categories ('Category'). Categories have
-- items ('Item') in them. Items have some sections (fields inside of
-- 'Item'), as well as traits ('Trait').
module Guide.Types.Core
(
  Trait(..),
  TraitLenses(..),
  TraitType (..),
  ItemKind(..),
    hackageName,
  ItemSection(..),
  Item(..),
  ItemLenses(..),
  CategoryStatus(..),
  Category(..),
  CategoryLenses(..),
  categorySlug,
)
where


import Imports

import Data.Functor.Contravariant ((>$<))
-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate

import Guide.Database.Utils
import Guide.Markdown
import Guide.Types.Hue
import Guide.Uid
import Guide.Utils

import qualified Data.Aeson as Aeson
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

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
  traitUid     :: Uid Trait,
  traitContent :: MarkdownInline }
  deriving (Show, Generic, Data, Eq, NFData)

deriveSafeCopySorted 4 'extension ''Trait
makeClassWithLenses ''Trait

changelog ''Trait (Current 4, Past 3) []
deriveSafeCopySorted 3 'base ''Trait_v3

instance Aeson.ToJSON Trait where
  toJSON $(fields 'Trait) = Aeson.object [
    "uid"             Aeson..= traitUid,
    "content"         Aeson..= traitContent
    ]

instance Aeson.FromJSON Trait where
  parseJSON = Aeson.withObject "Trait" $ \o -> do
    traitUid     <- o Aeson..: "uid"
    traitContent <- o Aeson..: "content"
    pure Trait{..}

-- | ADT for trait type. Traits can be pros (positive traits) and cons
-- (negative traits).
data TraitType = TraitTypePro | TraitTypeCon
  deriving (Eq, Show)

instance ToPostgres TraitType where
  toPostgres = HE.enum $ \case
    TraitTypePro -> "pro"
    TraitTypeCon -> "con"

instance FromPostgres TraitType where
  fromPostgres = HD.enum $ \case
    "pro" -> Just TraitTypePro
    "con" -> Just TraitTypeCon
    _ -> Nothing

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

instance Aeson.ToJSON ItemKind where
  toJSON (Library x) = Aeson.object [
    "tag"      Aeson..= ("Library" :: Text),
    "contents" Aeson..= x ]
  toJSON (Tool x) = Aeson.object [
    "tag"      Aeson..= ("Tool" :: Text),
    "contents" Aeson..= x ]
  toJSON Other = Aeson.object [
    "tag"      Aeson..= ("Other" :: Text) ]

instance Aeson.FromJSON ItemKind where
  parseJSON = Aeson.withObject "ItemKind" $ \o ->
      o Aeson..: "tag" >>= \case
        ("Library" :: Text) -> Library <$> o Aeson..: "contents"
        "Tool"    -> Tool <$> o Aeson..: "contents"
        "Other"   -> pure Other
        tag       -> fail ("unknown tag " ++ show tag)

data ItemKind_v2
  = Library_v2 (Maybe Text)
  | Tool_v2 (Maybe Text)
  | Other_v2

-- TODO: at the next migration change this to deriveSafeCopySimple!
deriveSafeCopy 2 'base ''ItemKind_v2

instance Migrate ItemKind where
  type MigrateFrom ItemKind = ItemKind_v2
  migrate (Library_v2 x) = Library x
  migrate (Tool_v2 x)    = Tool x
  migrate Other_v2       = Other

-- | Different kinds of sections inside items. This type is only used for
-- 'categoryEnabledSections'.
data ItemSection
  = ItemProsConsSection
  | ItemEcosystemSection
  | ItemNotesSection
  deriving (Eq, Ord, Show, Generic, Data, NFData)

deriveSafeCopySimple 0 'base ''ItemSection

instance Aeson.ToJSON ItemSection where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Aeson.FromJSON ItemSection where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToPostgres ItemSection where
  toPostgres = HE.enum $ \case
    ItemProsConsSection -> "pros_cons"
    ItemEcosystemSection -> "ecosystem"
    ItemNotesSection -> "notes"

instance FromPostgres ItemSection where
  fromPostgres = HD.enum $ \case
    "pros_cons" -> Just ItemProsConsSection
    "ecosystem" -> Just ItemEcosystemSection
    "notes" -> Just ItemNotesSection
    _ -> Nothing

-- TODO: add a field like “people to ask on IRC about this library if you
-- need help”

-- | An item (usually a library). Items are stored in categories.
data Item = Item {
  itemUid         :: Uid Item,        -- ^ Item ID
  itemName        :: Text,            -- ^ Item title
  itemCreated     :: UTCTime,         -- ^ When the item was created
  itemHackage     :: Maybe Text,      -- ^ Package name on Hackage
  itemSummary     :: MarkdownBlock,   -- ^ Item summary
  itemPros        :: [Trait],         -- ^ Pros (positive traits)
  itemProsDeleted :: [Trait],         -- ^ Deleted pros go here (so that
                                      --   it'd be easy to restore them)
  itemCons        :: [Trait],         -- ^ Cons (negative traits)
  itemConsDeleted :: [Trait],         -- ^ Deleted cons go here
  itemEcosystem   :: MarkdownBlock,   -- ^ The ecosystem section
  itemNotes       :: MarkdownTree,    -- ^ The notes section
  itemLink        :: Maybe Url        -- ^ Link to homepage or something
  }
  deriving (Generic, Data, Eq, Show, NFData)

deriveSafeCopySorted 13 'extension ''Item
makeClassWithLenses ''Item

changelog ''Item (Current 13, Past 12)
  [Removed "itemGroup_"  [t|Maybe Text|] ]
deriveSafeCopySorted 12 'extension ''Item_v12

changelog ''Item (Past 12, Past 11)
  [Removed "itemKind"  [t|ItemKind|],
   Added "itemHackage" [hs|
     case itemKind of
       Library m -> m
       Tool m -> m
       Other -> Nothing |],
   Removed "itemDescription" [t|MarkdownBlock|],
   Added "itemSummary" [hs|
     itemDescription |] ]
deriveSafeCopySorted 11 'extension ''Item_v11

changelog ''Item (Past 11, Past 10) []
deriveSafeCopySorted 10 'base ''Item_v10

instance Aeson.ToJSON Item where
  toJSON $(fields 'Item) = Aeson.object [
    "uid"           Aeson..= itemUid,
    "name"          Aeson..= itemName,
    "created"       Aeson..= itemCreated,
    "hackage"       Aeson..= itemHackage,
    "summary"       Aeson..= itemSummary,
    "pros"          Aeson..= itemPros,
    "prosDeleted"   Aeson..= itemProsDeleted,
    "cons"          Aeson..= itemCons,
    "consDeleted"   Aeson..= itemConsDeleted,
    "ecosystem"     Aeson..= itemEcosystem,
    "notes"         Aeson..= itemNotes,
    "link"          Aeson..= itemLink
    ]

instance Aeson.FromJSON Item where
  parseJSON = Aeson.withObject "Item" $ \o -> do
    itemUid         <- o Aeson..:  "uid"
    itemName        <- o Aeson..:  "name"
    itemCreated     <- o Aeson..:  "created"
    itemHackage     <- o Aeson..:? "hackage"
    itemSummary     <- o Aeson..:  "summary"
    itemPros        <- o Aeson..:  "pros"
    itemProsDeleted <- o Aeson..:  "prosDeleted"
    itemCons        <- o Aeson..:  "cons"
    itemConsDeleted <- o Aeson..:  "consDeleted"
    itemEcosystem   <- o Aeson..:  "ecosystem"
    itemNotes       <- o Aeson..:  "notes"
    itemLink        <- o Aeson..:? "link"
    pure Item{..}

----------------------------------------------------------------------------
-- Category
----------------------------------------------------------------------------

-- | Category status
data CategoryStatus
  = CategoryStub                -- ^ “Stub” = just created
  | CategoryWIP                 -- ^ “WIP” = work in progress
  | CategoryFinished            -- ^ “Finished” = complete or nearly complete
  deriving (Eq, Show, Generic, Data, NFData)

deriveSafeCopySimple 2 'extension ''CategoryStatus

instance Aeson.ToJSON CategoryStatus where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance Aeson.FromJSON CategoryStatus where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToPostgres CategoryStatus where
  toPostgres = HE.enum $ \case
    CategoryStub -> "stub"
    CategoryWIP -> "wip"
    CategoryFinished -> "finished"

instance FromPostgres CategoryStatus where
  fromPostgres = HD.enum $ \case
    "stub" -> Just CategoryStub
    "wip" -> Just CategoryWIP
    "finished" -> Just CategoryFinished
    _ -> Nothing

data CategoryStatus_v1
  = CategoryStub_v1
  | CategoryWIP_v1
  | CategoryMostlyDone_v1
  | CategoryFinished_v1

deriveSafeCopySimple 1 'base ''CategoryStatus_v1

instance Migrate CategoryStatus where
  type MigrateFrom CategoryStatus = CategoryStatus_v1
  migrate CategoryStub_v1       = CategoryStub
  migrate CategoryWIP_v1        = CategoryWIP
  migrate CategoryMostlyDone_v1 = CategoryFinished
  migrate CategoryFinished_v1   = CategoryFinished

-- | A category
data Category = Category {
  categoryUid             :: Uid Category,
  categoryTitle           :: Text,
  -- | When the category was created
  categoryCreated         :: UTCTime,
  -- | The “grandcategory” of the category (“meta”, “basics”, etc)
  categoryGroup           :: Text,
  categoryStatus          :: CategoryStatus,
  categoryNotes           :: MarkdownBlock,
  -- | Items stored in the category
  categoryItems           :: [Item],
  -- | Items that were deleted from the category. We keep them here to make
  -- it easier to restore them
  categoryItemsDeleted    :: [Item],
  -- | Enabled sections in this category. E.g, if this set contains
  -- 'ItemNotesSection', then notes will be shown for each item
  categoryEnabledSections :: Set ItemSection
  }
  deriving (Generic, Data, Eq, Show, NFData)

deriveSafeCopySorted 13 'extension ''Category
makeClassWithLenses ''Category

changelog ''Category (Current 13, Past 12)
  [Removed "categoryGroup_" [t|Text|]
  ,Added   "categoryGroup"  [hs|
     categoryGroup_ |] ]
deriveSafeCopySorted 12 'extension ''Category_v12

changelog ''Category (Past 12, Past 11)
  [Removed "categoryGroups" [t|Map Text Hue|] ]
deriveSafeCopySorted 11 'extension ''Category_v11

changelog ''Category (Past 11, Past 10)
  [Removed "categoryProsConsEnabled"  [t|Bool|],
   Removed "categoryEcosystemEnabled" [t|Bool|],
   Removed "categoryNotesEnabled"     [t|Bool|],
   Added   "categoryEnabledSections"  [hs|
     toSet $ concat
       [ [ItemProsConsSection  | categoryProsConsEnabled]
       , [ItemEcosystemSection | categoryEcosystemEnabled]
       , [ItemNotesSection     | categoryNotesEnabled] ] |] ]
deriveSafeCopySorted 10 'extension ''Category_v10

changelog ''Category (Past 10, Past 9)
  [Added "categoryNotesEnabled" [hs|True|]]
deriveSafeCopySorted 9 'extension ''Category_v9

changelog ''Category (Past 9, Past 8) []
deriveSafeCopySorted 8 'base ''Category_v8

instance Aeson.ToJSON Category where
  toJSON $(fields 'Category) = Aeson.object [
    "uid"             Aeson..= categoryUid,
    "title"           Aeson..= categoryTitle,
    "created"         Aeson..= categoryCreated,
    "group"           Aeson..= categoryGroup,
    "status"          Aeson..= categoryStatus,
    "notes"           Aeson..= categoryNotes,
    "items"           Aeson..= categoryItems,
    "itemsDeleted"    Aeson..= categoryItemsDeleted,
    "enabledSections" Aeson..= categoryEnabledSections
    ]

instance Aeson.FromJSON Category where
  parseJSON = Aeson.withObject "Category" $ \o -> do
    categoryUid             <- o Aeson..: "uid"
    categoryTitle           <- o Aeson..: "title"
    categoryCreated         <- o Aeson..: "created"
    categoryGroup           <- o Aeson..: "group"
    categoryStatus          <- o Aeson..: "status"
    categoryNotes           <- o Aeson..: "notes"
    categoryItems           <- o Aeson..: "items"
    categoryItemsDeleted    <- o Aeson..: "itemsDeleted"
    categoryEnabledSections <- o Aeson..: "enabledSections"
    pure Category{..}

-- | 'jsonbBytes' is used to report an error as Left
--   without using 'error' function.
instance ToPostgres Category where
  toPostgres = toByteString . Aeson.encode >$< HE.jsonbBytes

instance FromPostgres Category where
  fromPostgres = HD.jsonbBytes $
    either (Left . toText) (Right . id) . Aeson.eitherDecodeStrict

-- | Category identifier (used in URLs). E.g. for a category with title
-- “Performance optimization” and UID “t3c9hwzo” the slug would be
-- @performance-optimization-t3c9hwzo@.
categorySlug :: Category -> Text
categorySlug category =
  format "{}-{}" (makeSlug (categoryTitle category)) (categoryUid category)
