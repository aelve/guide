{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utilities for working with Postgres.
module Guide.Database.Utils
(
  -- * Generic queries
  -- ** Simple query functions
  queryRow,
  queryRows,
  execute,
  -- ** General query function
  makeStatement,
  -- ** Typeclasses for encoders and decoders
  ToPostgres (..),
  FromPostgres (..),
  -- ** Row conversion
  ToPostgresParam (..),
  FromPostgresColumn (..),
  ToPostgresParams (..),
  FromPostgresRow (..),
  -- ** One-element row newtypes
  SingleParam (..),
  SingleColumn (..),
)
where

import Imports
import Named
import Hasql.Statement
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Contravariant.Divisible (divided, lost, chosen)
import Generics.Eot (toEot, HasEot(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)

import qualified Data.Set as Set
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

import Guide.Utils (Uid (..))

----------------------------------------------------------------------------
-- Query functions
----------------------------------------------------------------------------

-- | A wrapper around 'Statement' using named parameters.
makeStatement
  :: "prepared" :! Bool  -- ^ Whether the query should be prepared
  -> "params" :! HE.Params a  -- ^ How to encode the parameters
  -> "result" :! HD.Result b  -- ^ How to decode the result
  -> ByteString  -- ^ Query
  -> Statement a b
makeStatement
  (arg #prepared -> prepared)
  (arg #params -> params)
  (arg #result -> result)
  sql
  =
  Statement sql params result prepared

-- | Fetch a single row from the database.
--
-- @
-- statement :: Statement (Uid Trait, Bool) (Maybe (Uid Trait, Text))
-- statement =
--   [queryRow|
--     SELECT uid, content
--     FROM traits
--     WHERE uid = $1
--       AND deleted = $2
--   |]
-- @
--
-- Uses @'ToPostgresParams' a@ for encoding and @'FromPostgresRow' b@ for
-- decoding. Returns @Statement a (Maybe b)@.
--
-- The resulting 'Statement' can be executed using 'Hasql.Session.statement'
-- from "Hasql.Session" or 'Hasql.Transaction.statement' from
-- "Hasql.Transaction".
--
-- To pass several parameters to the query or get several columns from the
-- query, use tuples. To pass only one parameter, use 'SingleParam'. To get
-- back only one column, use 'SingleColumn'. The following example
-- demonstrates both:
--
-- @
-- statement :: Statement (Uid Item) (Maybe (Uid Category))
-- statement = dimap SingleParam (fmap fromSingleColumn) $
--   [queryRow|
--     SELECT category_uid
--     FROM items
--     WHERE uid = $1
--   |]
-- @
--
-- See 'Data.Profunctor.lmap', 'Data.Profunctor.rmap',
-- 'Data.Profunctor.dimap' from "Data.Profunctor".
queryRow :: QuasiQuoter
queryRow = QuasiQuoter
  { quoteExp = \s ->
      [|makeStatement
          (#prepared False)
          (#params toPostgresParams)
          (#result (HD.rowMaybe fromPostgresRow))
          $(stringE s)|]
  , quotePat = error "queryRow: can not be used in patterns"
  , quoteType = error "queryRow: can not be used in types"
  , quoteDec = error "queryRow: can not be used in declarations"
  }

-- | Fetch many rows from the database.
--
-- Like 'queryRow', but returns @Statement a [b]@ instead of @Statement a
-- (Maybe b)@.
queryRows :: QuasiQuoter
queryRows = QuasiQuoter
  { quoteExp = \s ->
      [|makeStatement
          (#prepared False)
          (#params toPostgresParams)
          (#result (HD.rowList fromPostgresRow))
          $(stringE s)|]
  , quotePat = error "queryRows: can not be used in patterns"
  , quoteType = error "queryRows: can not be used in types"
  , quoteDec = error "queryRows: can not be used in declarations"
  }

-- | Execute a query without returning anything.
--
-- Like 'queryRow', but returns @Statement a ()@ instead of @Statement a
-- (Maybe b)@.
execute :: QuasiQuoter
execute = QuasiQuoter
  { quoteExp = \s ->
      [|makeStatement
          (#prepared False)
          (#params toPostgresParams)
          (#result HD.noResult)
          $(stringE s)|]
  , quotePat = error "execute: can not be used in patterns"
  , quoteType = error "execute: can not be used in types"
  , quoteDec = error "execute: can not be used in declarations"
  }

----------------------------------------------------------------------------
-- ToPostgres and FromPostgres
----------------------------------------------------------------------------

class ToPostgres a where
  -- | Encode a single value to the Postgres format.
  --
  -- If you have a newtype over an existing type supported by 'ToPostgres',
  -- you can write an instance like this:
  --
  -- @
  -- instance ToPostgres Foo where
  --   toPostgres = unFoo '>$<' toPostgres
  -- @
  toPostgres :: HE.Value a

class FromPostgres a where
  -- | Decode a single value from the Postgres format.
  --
  -- If you have a newtype over an existing type supported by
  -- 'FromPostgres', you can write an instance like this:
  --
  -- @
  -- instance FromPostgres Foo where
  --   fromPostgres = Foo '<$>' fromPostgres
  -- @
  fromPostgres :: HD.Value a

instance ToPostgres Bool where
  toPostgres = HE.bool

instance FromPostgres Bool where
  fromPostgres = HD.bool

instance ToPostgres Int32 where
  toPostgres = HE.int4

instance FromPostgres Int32 where
  fromPostgres = HD.int4

instance ToPostgres Text where
  toPostgres = HE.text

instance FromPostgres Text where
  fromPostgres = HD.text

instance ToPostgres UTCTime where
  toPostgres = HE.timestamptz

instance FromPostgres UTCTime where
  fromPostgres = HD.timestamptz

instance ToPostgres (Uid a) where
  toPostgres = uidToText >$< HE.text

instance FromPostgres (Uid a) where
  fromPostgres = Uid <$> HD.text

----------------------------------------------------------------------------
-- ToPostgresParam
----------------------------------------------------------------------------

class ToPostgresParam a where
  -- | Convert a single value to a parameter that can be passed to a query.
  --
  -- Unlike 'toPostgres', this function can deal with nullable parameters
  -- and array parameters, which are not representable as 'HE.Value'.
  toPostgresParam :: HE.Params a

-- | Non-nullable parameters.
instance {-# OVERLAPPABLE #-} ToPostgres a => ToPostgresParam a where
  toPostgresParam = HE.param (HE.nonNullable toPostgres)

-- | Nullable parameters.
instance ToPostgres a => ToPostgresParam (Maybe a) where
  toPostgresParam = HE.param (HE.nullable toPostgres)

-- | Arrays of non-nullable values.
instance ToPostgres a => ToPostgresParam [a] where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nonNullable toPostgres)))

-- | Arrays of nullable values.
instance ToPostgres a => ToPostgresParam [Maybe a] where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nullable toPostgres)))

-- | Sets of non-nullable values, represented as arrays.
instance ToPostgres a => ToPostgresParam (Set a) where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nonNullable toPostgres)))

-- | Sets of nullable values, represented as arrays.
instance ToPostgres a => ToPostgresParam (Set (Maybe a)) where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nullable toPostgres)))

----------------------------------------------------------------------------
-- FromPostgresColumn
----------------------------------------------------------------------------

class FromPostgresColumn a where
  -- | Fetch a single column from a row.
  --
  -- Unlike 'fromPostgres', this function can deal with nullable columns and
  -- array columns, which are not representable as 'HD.Value'.
  fromPostgresColumn :: HD.Row a

-- | Non-nullable columns.
instance {-# OVERLAPPABLE #-} FromPostgres a => FromPostgresColumn a where
  fromPostgresColumn = HD.column (HD.nonNullable fromPostgres)

-- | Nullable columns.
instance FromPostgres a => FromPostgresColumn (Maybe a) where
  fromPostgresColumn = HD.column (HD.nullable fromPostgres)

-- | Arrays of non-nullable values.
instance FromPostgres a => FromPostgresColumn [a] where
  fromPostgresColumn =
    HD.column (HD.nonNullable (HD.listArray (HD.nonNullable fromPostgres)))

-- | Arrays of nullable values.
instance FromPostgres a => FromPostgresColumn [Maybe a] where
  fromPostgresColumn =
    HD.column (HD.nonNullable (HD.listArray (HD.nullable fromPostgres)))

-- | Sets of non-nullable values, represented as arrays.
instance (Ord a, FromPostgres a) => FromPostgresColumn (Set a) where
  fromPostgresColumn =
    Set.fromList <$>
    HD.column (HD.nonNullable (HD.listArray (HD.nonNullable fromPostgres)))

-- | Sets of nullable values, represented as arrays.
instance (Ord a, FromPostgres a) => FromPostgresColumn (Set (Maybe a)) where
  fromPostgresColumn =
    Set.fromList <$>
    HD.column (HD.nonNullable (HD.listArray (HD.nullable fromPostgres)))

----------------------------------------------------------------------------
-- ToPostgresParams
----------------------------------------------------------------------------

class ToPostgresParams a where
  -- | Pass a row of parameters to a query.
  toPostgresParams :: HE.Params a

  -- | A default implementation for anything that implements 'Generic'.
  default toPostgresParams :: (HasEot a, GToPostgresParams (Eot a)) => HE.Params a
  toPostgresParams = toEot >$< genericToPostgresParams

class GToPostgresParams a where
  -- | A generic method to encode data types with one constructor as Hasql
  -- rows. See
  -- <https://generics-eot.readthedocs.io/en/stable/tutorial.html#eot-isomorphic-representations>
  -- to understand its implementation.
  genericToPostgresParams :: HE.Params a

-- An instance for zero fields.
instance GToPostgresParams () where
  genericToPostgresParams = HE.noParams

-- If we can encode N fields, we can encode N+1 fields.
instance (ToPostgresParam a, GToPostgresParams b) => GToPostgresParams (a, b) where
  genericToPostgresParams = divided toPostgresParam genericToPostgresParams

-- One-constructor types are represented as @Either a Void@, where @a@ is a
-- tuple that we already know how to encode thanks to the instances above.
-- We do not support types with more than one constructor.
instance GToPostgresParams a => GToPostgresParams (Either a Void) where
  genericToPostgresParams = chosen genericToPostgresParams lost

----------------------------------------------------------------------------
-- FromPostgresRow
----------------------------------------------------------------------------

class FromPostgresRow a where
  -- | Fetch a row of results from a query.
  fromPostgresRow :: HD.Row a

  -- | A default implementation for anything that implements 'Generic'.
  default fromPostgresRow :: (HasEot a, GFromPostgresRow (Eot a)) => HD.Row a
  fromPostgresRow = fromEot <$> genericFromPostgresRow

class GFromPostgresRow a where
  -- | A generic method to decode data types with one constructor as Hasql
  -- rows. See
  -- <https://generics-eot.readthedocs.io/en/stable/tutorial.html#eot-isomorphic-representations>
  -- to understand its implementation.
  genericFromPostgresRow :: HD.Row a

-- An instance for zero fields.
instance GFromPostgresRow () where
  genericFromPostgresRow = pure ()

-- If we can encode N fields, we can encode N+1 fields.
instance (FromPostgresColumn a, GFromPostgresRow b) => GFromPostgresRow (a, b) where
  genericFromPostgresRow = (,) <$> fromPostgresColumn <*> genericFromPostgresRow

-- One-constructor types are represented as @Either a Void@, where @a@ is a
-- tuple that we already know how to encode thanks to the instances above.
-- We do not support types with more than one constructor.
instance GFromPostgresRow a => GFromPostgresRow (Either a Void) where
  genericFromPostgresRow = Left <$> genericFromPostgresRow

----------------------------------------------------------------------------
-- SingleParam and SingleColumn
----------------------------------------------------------------------------

newtype SingleParam a = SingleParam { fromSingleParam :: a }
  deriving (Eq, Ord, Show, Generic)

instance ToPostgresParam a => ToPostgresParams (SingleParam a)

newtype SingleColumn a = SingleColumn { fromSingleColumn :: a }
  deriving (Eq, Ord, Show, Generic)

instance FromPostgresColumn a => FromPostgresRow (SingleColumn a)

----------------------------------------------------------------------------
-- Tuples
----------------------------------------------------------------------------

-- Note: 'Generic' provides instances for tuples only until 7-tuples

instance ToPostgresParams ()
instance (ToPostgresParam a, ToPostgresParam b) => ToPostgresParams (a, b)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c) => ToPostgresParams (a, b, c)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d) => ToPostgresParams (a, b, c, d)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d, ToPostgresParam e) => ToPostgresParams (a, b, c, d, e)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d, ToPostgresParam e, ToPostgresParam f) => ToPostgresParams (a, b, c, d, e, f)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d, ToPostgresParam e, ToPostgresParam f, ToPostgresParam g) => ToPostgresParams (a, b, c, d, e, f, g)

instance FromPostgresRow ()
instance (FromPostgresColumn a, FromPostgresColumn b) => FromPostgresRow (a, b)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c) => FromPostgresRow (a, b, c)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d) => FromPostgresRow (a, b, c, d)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d, FromPostgresColumn e) => FromPostgresRow (a, b, c, d, e)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d, FromPostgresColumn e, FromPostgresColumn f) => FromPostgresRow (a, b, c, d, e, f)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d, FromPostgresColumn e, FromPostgresColumn f, FromPostgresColumn g) => FromPostgresRow (a, b, c, d, e, f, g)
