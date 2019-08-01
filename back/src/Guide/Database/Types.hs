-- | Types for postgres database
module Guide.Database.Types
       (
         DatabaseError(..)
       ) where

import Imports

import Guide.Types.Core (Category (..), Item (..), Trait(..))
import Guide.Utils (Uid (..))


-- | Custom datatype errors for database
data DatabaseError
  = ItemNotFound (Uid Item)
  | CategoryNotFound (Uid Category)
  | TraitNotFound (Uid Trait)
  | ElementIdIsInOrder
  | ElementIdIsNotInOrder
  deriving Show
