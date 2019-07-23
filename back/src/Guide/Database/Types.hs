-- | Types for postgres database
module Guide.Database.Types
       (
         DatabaseError(..)
       , DatabaseMonad
       , toDatabaseMonad
       ) where

import Imports

import Guide.Types.Core (Category (..), Item (..))
import Guide.Utils (Uid (..))

import Hasql.Session (Session)

-- | Custom datatype errors for database
data DatabaseError
  = ItemNotFound (Uid Item)
  | CategoryNotFound (Uid Category)
  deriving Show

-- | ExceptT wrapper on Session with custom errors
type DatabaseMonad = ExceptT DatabaseError Session

-- | Convert Session to DatabaseMonad
toDatabaseMonad :: Session a -> ExceptT DatabaseError Session a
toDatabaseMonad = ExceptT . fmap pure
