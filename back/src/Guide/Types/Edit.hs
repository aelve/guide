{-# LANGUAGE TypeFamilies #-}


{- |
Types for edits.

Every content edit is associated with an 'Edit', which is stored in the
database and can be undone. In addition, each edit has a corresponding
'EditDetails' (which stores IP, date, and ID of an edit).
-}
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
  = Edit'AddCategory {
      editCategoryUid   :: Uid Category,
      editCategoryTitle :: Text,
      editCategoryGroup :: Text }
  | Edit'AddItem {
      editCategoryUid :: Uid Category,
      editItemUid     :: Uid Item,
      editItemName    :: Text }
  | Edit'AddPro {
      editItemUid      :: Uid Item,
      editTraitId      :: Uid Trait,
      editTraitContent :: Text }
  | Edit'AddCon {
      editItemUid      :: Uid Item,
      editTraitId      :: Uid Trait,
      editTraitContent :: Text }

  -- Change category properties
  | Edit'SetCategoryTitle {
      editCategoryUid      :: Uid Category,
      editCategoryTitle    :: Text,
      editCategoryNewTitle :: Text }
  | Edit'SetCategoryGroup {
      editCategoryUid      :: Uid Category,
      editCategoryGroup    :: Text,
      editCategoryNewGroup :: Text }
  | Edit'SetCategoryNotes {
      editCategoryUid      :: Uid Category,
      editCategoryNotes    :: Text,
      editCategoryNewNotes :: Text }
  | Edit'SetCategoryStatus {
      editCategoryUid       :: Uid Category,
      editCategoryStatus    :: CategoryStatus,
      editCategoryNewStatus :: CategoryStatus }
  | Edit'ChangeCategoryEnabledSections {
      editCategoryUid             :: Uid Category,
      editCategoryEnableSections  :: Set ItemSection,
      editCategoryDisableSections :: Set ItemSection }

  -- Change item properties
  | Edit'SetItemName {
      editItemUid     :: Uid Item,
      editItemName    :: Text,
      editItemNewName :: Text }
  | Edit'SetItemLink {
      editItemUid     :: Uid Item,
      editItemLink    :: Maybe Url,
      editItemNewLink :: Maybe Url }
  | Edit'SetItemGroup {
      editItemUid      :: Uid Item,
      editItemGroup    :: Maybe Text,
      editItemNewGroup :: Maybe Text }
  | Edit'SetItemHackage {
      editItemUid        :: Uid Item,
      editItemHackage    :: Maybe Text,
      editItemNewHackage :: Maybe Text }

  | Edit'SetItemSummary {
      editItemUid        :: Uid Item,
      editItemSummary    :: Text,
      editItemNewSummary :: Text }
  | Edit'SetItemNotes {
      editItemUid      :: Uid Item,
      editItemNotes    :: Text,
      editItemNewNotes :: Text }
  | Edit'SetItemEcosystem {
      editItemUid          :: Uid Item,
      editItemEcosystem    :: Text,
      editItemNewEcosystem :: Text }

  -- Change trait properties
  | Edit'SetTraitContent {
      editItemUid         :: Uid Item,
      editTraitUid        :: Uid Trait,
      editTraitContent    :: Text,
      editTraitNewContent :: Text }

  -- Delete
  | Edit'DeleteCategory {
      editCategoryUid      :: Uid Category,
      editCategoryPosition :: Int }
  | Edit'DeleteItem {
      editItemUid      :: Uid Item,
      editItemPosition :: Int }
  | Edit'DeleteTrait {
      editItemUid       :: Uid Item,
      editTraitUid      :: Uid Trait,
      editTraitPosition :: Int }

  -- Other
  | Edit'MoveItem {
      editItemUid   :: Uid Item,
      editDirection :: Bool }
  | Edit'MoveTrait {
      editItemUid   :: Uid Item,
      editTraitUid  :: Uid Trait,
      editDirection :: Bool }

  deriving (Eq, Show)

deriveSafeCopySimple 9 'extension ''Edit

genVer ''Edit (Current 9, Past 8) [
  -- Add
  Copy "Edit'AddCategory",
  Copy "Edit'AddItem",
  Copy "Edit'AddPro",
  Copy "Edit'AddCon",
  -- Change category properties
  Copy "Edit'SetCategoryTitle",
  Copy "Edit'SetCategoryGroup",
  Copy "Edit'SetCategoryNotes",
  Copy "Edit'SetCategoryStatus",
  Copy "Edit'ChangeCategoryEnabledSections",
  -- Change item properties
  Copy "Edit'SetItemName",
  Copy "Edit'SetItemLink",
  Copy "Edit'SetItemGroup",
  Custom "Edit'SetItemKind" [
    ("editItemUid", [t|Uid Item|]),
    ("editItemKind", [t|ItemKind|]),
    ("editItemNewKind", [t|ItemKind|])],
  Copy "Edit'SetItemSummary",
  Copy "Edit'SetItemNotes",
  Copy "Edit'SetItemEcosystem",
  -- Change trait properties
  Copy "Edit'SetTraitContent",
  -- Delete
  Copy "Edit'DeleteCategory",
  Copy "Edit'DeleteItem",
  Copy "Edit'DeleteTrait",
  -- Other
  Copy "Edit'MoveItem",
  Copy "Edit'MoveTrait" ]

deriveSafeCopySimple 8 'extension ''Edit_v8

instance Migrate Edit where
  type MigrateFrom Edit = Edit_v8
  migrate = $(migrateVer ''Edit (Current 9, Past 8) [
    -- Add
    CopyM "Edit'AddCategory",
    CopyM "Edit'AddItem",
    CopyM "Edit'AddPro",
    CopyM "Edit'AddCon",
    -- Change category properties
    CopyM "Edit'SetCategoryTitle",
    CopyM "Edit'SetCategoryGroup",
    CopyM "Edit'SetCategoryNotes",
    CopyM "Edit'SetCategoryStatus",
    CopyM "Edit'ChangeCategoryEnabledSections",
    -- Change item properties
    CopyM "Edit'SetItemName",
    CopyM "Edit'SetItemLink",
    CopyM "Edit'SetItemGroup",
    CustomM "Edit'SetItemKind" [|\x ->
      Edit'SetItemHackage
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
    CopyM "Edit'SetItemSummary",
    CopyM "Edit'SetItemNotes",
    CopyM "Edit'SetItemEcosystem",
    -- Change trait properties
    CopyM "Edit'SetTraitContent",
    -- Delete
    CopyM "Edit'DeleteCategory",
    CopyM "Edit'DeleteItem",
    CopyM "Edit'DeleteTrait",
    -- Other
    CopyM "Edit'MoveItem",
    CopyM "Edit'MoveTrait"
    ])

genVer ''Edit (Past 8, Past 7) [
  -- Add
  Custom "Edit'AddCategory" [
      ("editCategoryUid"  , [t|Uid Category|]),
      ("editCategoryTitle", [t|Text|]) ],
  Copy "Edit'AddItem",
  Copy "Edit'AddPro",
  Copy "Edit'AddCon",
  -- Change category properties
  Copy "Edit'SetCategoryTitle",
  Copy "Edit'SetCategoryGroup",
  Copy "Edit'SetCategoryNotes",
  Copy "Edit'SetCategoryStatus",
  Copy "Edit'ChangeCategoryEnabledSections",
  -- Change item properties
  Copy "Edit'SetItemName",
  Copy "Edit'SetItemLink",
  Copy "Edit'SetItemGroup",
  Copy "Edit'SetItemKind",
  Copy "Edit'SetItemSummary",
  Copy "Edit'SetItemNotes",
  Copy "Edit'SetItemEcosystem",
  -- Change trait properties
  Copy "Edit'SetTraitContent",
  -- Delete
  Copy "Edit'DeleteCategory",
  Copy "Edit'DeleteItem",
  Copy "Edit'DeleteTrait",
  -- Other
  Copy "Edit'MoveItem",
  Copy "Edit'MoveTrait" ]

deriveSafeCopySimple 7 'base ''Edit_v7

instance Migrate Edit_v8 where
  type MigrateFrom Edit_v8 = Edit_v7
  migrate = $(migrateVer ''Edit (Past 8, Past 7) [
    CustomM "Edit'AddCategory" [|\x ->
      Edit'AddCategory_v8
        { editCategoryUid_v8 = editCategoryUid_v7 x
        , editCategoryTitle_v8 = editCategoryTitle_v7 x
        , editCategoryGroup_v8 = toText "Miscellaneous"
        } |],
    CopyM "Edit'AddItem",
    CopyM "Edit'AddPro",
    CopyM "Edit'AddCon",
    -- Change category properties
    CopyM "Edit'SetCategoryTitle",
    CopyM "Edit'SetCategoryGroup",
    CopyM "Edit'SetCategoryNotes",
    CopyM "Edit'SetCategoryStatus",
    CopyM "Edit'ChangeCategoryEnabledSections",
    -- Change item properties
    CopyM "Edit'SetItemName",
    CopyM "Edit'SetItemLink",
    CopyM "Edit'SetItemGroup",
    CopyM "Edit'SetItemKind",
    CopyM "Edit'SetItemSummary",
    CopyM "Edit'SetItemNotes",
    CopyM "Edit'SetItemEcosystem",
    -- Change trait properties
    CopyM "Edit'SetTraitContent",
    -- Delete
    CopyM "Edit'DeleteCategory",
    CopyM "Edit'DeleteItem",
    CopyM "Edit'DeleteTrait",
    -- Other
    CopyM "Edit'MoveItem",
    CopyM "Edit'MoveTrait"
    ])

-- | Determine whether the edit doesn't actually change anything and so isn't
-- worth recording in the list of pending edits.
isVacuousEdit :: Edit -> Bool
isVacuousEdit Edit'SetCategoryTitle {..} =
  editCategoryTitle == editCategoryNewTitle
isVacuousEdit Edit'SetCategoryGroup {..} =
  editCategoryGroup == editCategoryNewGroup
isVacuousEdit Edit'SetCategoryNotes {..} =
  editCategoryNotes == editCategoryNewNotes
isVacuousEdit Edit'SetCategoryStatus {..} =
  editCategoryStatus == editCategoryNewStatus
isVacuousEdit Edit'ChangeCategoryEnabledSections {..} =
  null editCategoryEnableSections && null editCategoryDisableSections
isVacuousEdit Edit'SetItemName {..} =
  editItemName == editItemNewName
isVacuousEdit Edit'SetItemLink {..} =
  editItemLink == editItemNewLink
isVacuousEdit Edit'SetItemGroup {..} =
  editItemGroup == editItemNewGroup
isVacuousEdit Edit'SetItemHackage {..} =
  editItemHackage == editItemNewHackage
isVacuousEdit Edit'SetItemSummary {..} =
  editItemSummary == editItemNewSummary
isVacuousEdit Edit'SetItemNotes {..} =
  editItemNotes == editItemNewNotes
isVacuousEdit Edit'SetItemEcosystem {..} =
  editItemEcosystem == editItemNewEcosystem
isVacuousEdit Edit'SetTraitContent {..} =
  editTraitContent == editTraitNewContent
isVacuousEdit Edit'AddCategory{}    = False
isVacuousEdit Edit'AddItem{}        = False
isVacuousEdit Edit'AddPro{}         = False
isVacuousEdit Edit'AddCon{}         = False
isVacuousEdit Edit'DeleteCategory{} = False
isVacuousEdit Edit'DeleteItem{}     = False
isVacuousEdit Edit'DeleteTrait{}    = False
isVacuousEdit Edit'MoveItem{}       = False
isVacuousEdit Edit'MoveTrait{}      = False

data EditDetails = EditDetails {
  editIP   :: Maybe IP,
  editDate :: UTCTime,
  editId   :: Int }
  deriving (Eq, Show)

deriveSafeCopySorted 4 'extension ''EditDetails

changelog ''EditDetails (Current 4, Past 3) []
deriveSafeCopySorted 3 'base ''EditDetails_v3
