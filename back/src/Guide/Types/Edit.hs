{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TemplateHaskell #-}


-- | Types for edits.
--
-- Every content edit is associated with an 'Edit', which is stored in the
-- database and can be undone. In addition, each edit has a corresponding
-- 'EditDetails' (which stores IP, date, and ID of an edit).
module Guide.Types.Edit
(
  Edit(..),
  isVacuousEdit,
  EditDetails(..),
)
where


import Imports

-- Network
import Data.IP
-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate

import Guide.Types.Core
import Guide.Utils


-- | Edits made by users. It should always be possible to undo an edit.
data Edit
  -- Add
  = EditAddCategory {
      editCategoryUid   :: Uid Category,
      editCategoryTitle :: Text,
      editCategoryGroup :: Text }
  | EditAddItem {
      editCategoryUid :: Uid Category,
      editItemUid     :: Uid Item,
      editItemName    :: Text }
  | EditAddPro {
      editItemUid      :: Uid Item,
      editTraitId      :: Uid Trait,
      editTraitContent :: Text }
  | EditAddCon {
      editItemUid      :: Uid Item,
      editTraitId      :: Uid Trait,
      editTraitContent :: Text }

  -- Change category properties
  | EditSetCategoryTitle {
      editCategoryUid      :: Uid Category,
      editCategoryTitle    :: Text,
      editCategoryNewTitle :: Text }
  | EditSetCategoryGroup {
      editCategoryUid      :: Uid Category,
      editCategoryGroup    :: Text,
      editCategoryNewGroup :: Text }
  | EditSetCategoryNotes {
      editCategoryUid      :: Uid Category,
      editCategoryNotes    :: Text,
      editCategoryNewNotes :: Text }
  | EditSetCategoryStatus {
      editCategoryUid       :: Uid Category,
      editCategoryStatus    :: CategoryStatus,
      editCategoryNewStatus :: CategoryStatus }
  | EditChangeCategoryEnabledSections {
      editCategoryUid             :: Uid Category,
      editCategoryEnableSections  :: Set ItemSection,
      editCategoryDisableSections :: Set ItemSection }

  -- Change item properties
  | EditSetItemName {
      editItemUid     :: Uid Item,
      editItemName    :: Text,
      editItemNewName :: Text }
  | EditSetItemLink {
      editItemUid     :: Uid Item,
      editItemLink    :: Maybe Url,
      editItemNewLink :: Maybe Url }
  | EditSetItemGroup {  -- TODO: remove after migration to Postgres
      editItemUid      :: Uid Item,
      editItemGroup    :: Maybe Text,
      editItemNewGroup :: Maybe Text }
  | EditSetItemHackage {
      editItemUid        :: Uid Item,
      editItemHackage    :: Maybe Text,
      editItemNewHackage :: Maybe Text }

  | EditSetItemSummary {
      editItemUid        :: Uid Item,
      editItemSummary    :: Text,
      editItemNewSummary :: Text }
  | EditSetItemNotes {
      editItemUid      :: Uid Item,
      editItemNotes    :: Text,
      editItemNewNotes :: Text }
  | EditSetItemEcosystem {
      editItemUid          :: Uid Item,
      editItemEcosystem    :: Text,
      editItemNewEcosystem :: Text }

  -- Change trait properties
  | EditSetTraitContent {
      editItemUid         :: Uid Item,
      editTraitUid        :: Uid Trait,
      editTraitContent    :: Text,
      editTraitNewContent :: Text }

  -- Delete
  | EditDeleteCategory {
      editCategoryUid      :: Uid Category,
      editCategoryPosition :: Int }
  | EditDeleteItem {
      editItemUid      :: Uid Item,
      editItemPosition :: Int }
  | EditDeleteTrait {
      editItemUid       :: Uid Item,
      editTraitUid      :: Uid Trait,
      editTraitPosition :: Int }

  -- Other
  | EditMoveItem {
      editItemUid   :: Uid Item,
      editDirection :: Bool }
  | EditMoveTrait {
      editItemUid   :: Uid Item,
      editTraitUid  :: Uid Trait,
      editDirection :: Bool }

  deriving (Eq, Show)

deriveSafeCopySimple 9 'extension ''Edit

genVer ''Edit (Current 9, Past 8) [
  -- Add
  Copy "EditAddCategory",
  Copy "EditAddItem",
  Copy "EditAddPro",
  Copy "EditAddCon",
  -- Change category properties
  Copy "EditSetCategoryTitle",
  Copy "EditSetCategoryGroup",
  Copy "EditSetCategoryNotes",
  Copy "EditSetCategoryStatus",
  Copy "EditChangeCategoryEnabledSections",
  -- Change item properties
  Copy "EditSetItemName",
  Copy "EditSetItemLink",
  Copy "EditSetItemGroup",
  Custom "EditSetItemKind" [
    ("editItemUid", [t|Uid Item|]),
    ("editItemKind", [t|ItemKind|]),
    ("editItemNewKind", [t|ItemKind|])],
  Copy "EditSetItemSummary",
  Copy "EditSetItemNotes",
  Copy "EditSetItemEcosystem",
  -- Change trait properties
  Copy "EditSetTraitContent",
  -- Delete
  Copy "EditDeleteCategory",
  Copy "EditDeleteItem",
  Copy "EditDeleteTrait",
  -- Other
  Copy "EditMoveItem",
  Copy "EditMoveTrait" ]

deriveSafeCopySimple 8 'extension ''Edit_v8

instance Migrate Edit where
  type MigrateFrom Edit = Edit_v8
  migrate = $(migrateVer ''Edit (Current 9, Past 8) [
    -- Add
    CopyM "EditAddCategory",
    CopyM "EditAddItem",
    CopyM "EditAddPro",
    CopyM "EditAddCon",
    -- Change category properties
    CopyM "EditSetCategoryTitle",
    CopyM "EditSetCategoryGroup",
    CopyM "EditSetCategoryNotes",
    CopyM "EditSetCategoryStatus",
    CopyM "EditChangeCategoryEnabledSections",
    -- Change item properties
    CopyM "EditSetItemName",
    CopyM "EditSetItemLink",
    CopyM "EditSetItemGroup",
    CustomM "EditSetItemKind" [|\x ->
      EditSetItemHackage
        { editItemUid = editItemUid_v8 x
        , editItemHackage = case editItemKind_v8 x of
          Library m -> m
          Tool m    -> m
          Other     -> Nothing
        , editItemNewHackage  = case editItemNewKind_v8 x of
          Library m -> m
          Tool m    -> m
          Other     -> Nothing
        }
    |],
    CopyM "EditSetItemSummary",
    CopyM "EditSetItemNotes",
    CopyM "EditSetItemEcosystem",
    -- Change trait properties
    CopyM "EditSetTraitContent",
    -- Delete
    CopyM "EditDeleteCategory",
    CopyM "EditDeleteItem",
    CopyM "EditDeleteTrait",
    -- Other
    CopyM "EditMoveItem",
    CopyM "EditMoveTrait"
    ])

genVer ''Edit (Past 8, Past 7) [
  -- Add
  Custom "EditAddCategory" [
      ("editCategoryUid"  , [t|Uid Category|]),
      ("editCategoryTitle", [t|Text|]) ],
  Copy "EditAddItem",
  Copy "EditAddPro",
  Copy "EditAddCon",
  -- Change category properties
  Copy "EditSetCategoryTitle",
  Copy "EditSetCategoryGroup",
  Copy "EditSetCategoryNotes",
  Copy "EditSetCategoryStatus",
  Copy "EditChangeCategoryEnabledSections",
  -- Change item properties
  Copy "EditSetItemName",
  Copy "EditSetItemLink",
  Copy "EditSetItemGroup",
  Copy "EditSetItemKind",
  Copy "EditSetItemSummary",
  Copy "EditSetItemNotes",
  Copy "EditSetItemEcosystem",
  -- Change trait properties
  Copy "EditSetTraitContent",
  -- Delete
  Copy "EditDeleteCategory",
  Copy "EditDeleteItem",
  Copy "EditDeleteTrait",
  -- Other
  Copy "EditMoveItem",
  Copy "EditMoveTrait" ]

deriveSafeCopySimple 7 'base ''Edit_v7

instance Migrate Edit_v8 where
  type MigrateFrom Edit_v8 = Edit_v7
  migrate = $(migrateVer ''Edit (Past 8, Past 7) [
    CustomM "EditAddCategory" [|\x ->
      EditAddCategory_v8
        { editCategoryUid_v8 = editCategoryUid_v7 x
        , editCategoryTitle_v8 = editCategoryTitle_v7 x
        , editCategoryGroup_v8 = toText "Miscellaneous"
        } |],
    CopyM "EditAddItem",
    CopyM "EditAddPro",
    CopyM "EditAddCon",
    -- Change category properties
    CopyM "EditSetCategoryTitle",
    CopyM "EditSetCategoryGroup",
    CopyM "EditSetCategoryNotes",
    CopyM "EditSetCategoryStatus",
    CopyM "EditChangeCategoryEnabledSections",
    -- Change item properties
    CopyM "EditSetItemName",
    CopyM "EditSetItemLink",
    CopyM "EditSetItemGroup",
    CopyM "EditSetItemKind",
    CopyM "EditSetItemSummary",
    CopyM "EditSetItemNotes",
    CopyM "EditSetItemEcosystem",
    -- Change trait properties
    CopyM "EditSetTraitContent",
    -- Delete
    CopyM "EditDeleteCategory",
    CopyM "EditDeleteItem",
    CopyM "EditDeleteTrait",
    -- Other
    CopyM "EditMoveItem",
    CopyM "EditMoveTrait"
    ])

-- | Determine whether the edit doesn't actually change anything and so isn't
-- worth recording in the list of pending edits.
isVacuousEdit :: Edit -> Bool
isVacuousEdit EditSetCategoryTitle {..} =
  editCategoryTitle == editCategoryNewTitle
isVacuousEdit EditSetCategoryGroup {..} =
  editCategoryGroup == editCategoryNewGroup
isVacuousEdit EditSetCategoryNotes {..} =
  editCategoryNotes == editCategoryNewNotes
isVacuousEdit EditSetCategoryStatus {..} =
  editCategoryStatus == editCategoryNewStatus
isVacuousEdit EditChangeCategoryEnabledSections {..} =
  null editCategoryEnableSections && null editCategoryDisableSections
isVacuousEdit EditSetItemName {..} =
  editItemName == editItemNewName
isVacuousEdit EditSetItemLink {..} =
  editItemLink == editItemNewLink
isVacuousEdit EditSetItemGroup {..} =
  editItemGroup == editItemNewGroup
isVacuousEdit EditSetItemHackage {..} =
  editItemHackage == editItemNewHackage
isVacuousEdit EditSetItemSummary {..} =
  editItemSummary == editItemNewSummary
isVacuousEdit EditSetItemNotes {..} =
  editItemNotes == editItemNewNotes
isVacuousEdit EditSetItemEcosystem {..} =
  editItemEcosystem == editItemNewEcosystem
isVacuousEdit EditSetTraitContent {..} =
  editTraitContent == editTraitNewContent
isVacuousEdit EditAddCategory{}    = False
isVacuousEdit EditAddItem{}        = False
isVacuousEdit EditAddPro{}         = False
isVacuousEdit EditAddCon{}         = False
isVacuousEdit EditDeleteCategory{} = False
isVacuousEdit EditDeleteItem{}     = False
isVacuousEdit EditDeleteTrait{}    = False
isVacuousEdit EditMoveItem{}       = False
isVacuousEdit EditMoveTrait{}      = False

data EditDetails = EditDetails {
  editIP   :: Maybe IP,
  editDate :: UTCTime,
  editId   :: Int }
  deriving (Eq, Show)

deriveSafeCopySorted 4 'extension ''EditDetails

changelog ''EditDetails (Current 4, Past 3) []
deriveSafeCopySorted 3 'base ''EditDetails_v3
