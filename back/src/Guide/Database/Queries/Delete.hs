-- | Delete queries.
module Guide.Database.Queries.Delete
(
  deleteCategory,
  deleteItem,
  deleteTrait,
)
where

import Imports

import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Data.Profunctor (lmap)

import qualified Hasql.Transaction as HT

import Guide.Database.Queries.Select
import Guide.Database.Queries.Update
import Guide.Database.Types
import Guide.Database.Utils
import Guide.Types.Core
import Guide.Utils (Uid (..))

-- | Delete a category completly.
deleteCategory :: Uid Category -> ExceptT DatabaseError Transaction ()
deleteCategory catId = do
  let statement :: Statement (Uid Category) ()
      statement = lmap SingleParam $
        [execute|
          DELETE FROM categories
          WHERE uid = $1
        |]
  lift $ HT.statement catId statement
  -- Items belonging to the category will be deleted automatically because
  -- of "ON DELETE CASCADE" in the table schema.

-- | Delete an item completly.
deleteItem :: Uid Item -> ExceptT DatabaseError Transaction ()
deleteItem itemId = do
  catId <- selectCategoryIdByItem itemId
  let statement :: Statement (Uid Item) ()
      statement = lmap SingleParam $
        [execute|
          DELETE FROM items
          WHERE uid = $1
        |]
  lift $ HT.statement itemId statement
  updateCategoryRow catId $
    _categoryRowItemsOrder %~ delete itemId
  -- Traits belonging to the item will be deleted automatically because of
  -- "ON DELETE CASCADE" in the table schema.

-- | Delete a trait completly.
deleteTrait :: Uid Trait -> ExceptT DatabaseError Transaction ()
deleteTrait traitId = do
  itemId <- selectItemIdByTrait traitId
  traitType <- traitRowType <$> selectTraitRow traitId
  let statement :: Statement (Uid Trait) ()
      statement = lmap SingleParam $
        [execute|
          DELETE FROM traits
          WHERE uid = $1
        |]
  lift $ HT.statement traitId statement
  case traitType of
    TraitTypePro ->
      updateItemRow itemId $
        _itemRowProsOrder %~ delete traitId
    TraitTypeCon ->
      updateItemRow itemId $
        _itemRowConsOrder %~ delete traitId
