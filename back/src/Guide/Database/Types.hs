-- | Types for postgres database
module Guide.Database.Types
       (
         DatabaseError(..)

       ) where

import Imports

import Guide.Types.Core
import Guide.Uid


-- | Custom datatype errors for database
data DatabaseError
  = CategoryNotFound (Uid Category)
  deriving Show


