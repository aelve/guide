{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Guide.Database.Utils
(
  makeStatement,
  query,
  queryMany,
  execute,
  ToPostgres (..),
  FromPostgres (..),
  ToPostgresParam (..),
  FromPostgresColumn (..),
  ToPostgresParams (..),
  FromPostgresRow (..),
)
where

import Imports
import Named
import Hasql.Statement
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Contravariant.Divisible (divided, lost, chosen)
import Generics.Eot

import qualified Data.Set as Set
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

import Guide.Utils (Uid (..))

----------------------------------------------------------------------------
-- makeStatement
----------------------------------------------------------------------------

makeStatement
  :: ToPostgresParams a
  => "prepared" :! Bool
  -> "result" :! HD.Result b
  -> ByteString
  -> Statement a b
makeStatement (arg #prepared -> prepared) (arg #result -> result) sql =
  Statement sql toPostgresParams result prepared

----------------------------------------------------------------------------
-- Easier versions
----------------------------------------------------------------------------

-- | Fetch a single row from the database.
query :: (ToPostgresParams a, FromPostgresRow b) => ByteString -> Statement a (Maybe b)
query = makeStatement (#prepared False) (#result (HD.rowMaybe fromPostgresRow))

-- | Fetch many rows from the database.
queryMany :: (ToPostgresParams a, FromPostgresRow b) => ByteString -> Statement a [b]
queryMany = makeStatement (#prepared False) (#result (HD.rowList fromPostgresRow))

-- | Execute a query without returning anything.
execute :: ToPostgresParams a => ByteString -> Statement a ()
execute = makeStatement (#prepared False) (#result HD.noResult)

----------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------

class ToPostgres a where
  toPostgres :: HE.Value a

class FromPostgres a where
  fromPostgres :: HD.Value a

class ToPostgresParam a where
  toPostgresParam :: HE.Params a

class FromPostgresColumn a where
  fromPostgresColumn :: HD.Row a

class ToPostgresParams a where
  toPostgresParams :: HE.Params a

  default toPostgresParams :: (HasEot a, GToPostgresParams (Eot a)) => HE.Params a
  toPostgresParams = toEot @a >$< genericToPostgresParams

class FromPostgresRow a where
  fromPostgresRow :: HD.Row a

  default fromPostgresRow :: (HasEot a, GFromPostgresRow (Eot a)) => HD.Row a
  fromPostgresRow = fromEot <$> genericFromPostgresRow

----------------------------------------------------------------------------
-- ToPostgresParam
----------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} ToPostgres a => ToPostgresParam a where
  toPostgresParam = HE.param (HE.nonNullable toPostgres)

instance ToPostgres a => ToPostgresParam (Maybe a) where
  toPostgresParam = HE.param (HE.nullable toPostgres)

instance ToPostgres a => ToPostgresParam [a] where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nonNullable toPostgres)))

instance ToPostgres a => ToPostgresParam [Maybe a] where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nullable toPostgres)))

instance ToPostgres a => ToPostgresParam (Set a) where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nonNullable toPostgres)))

instance ToPostgres a => ToPostgresParam (Set (Maybe a)) where
  toPostgresParam =
    HE.param (HE.nonNullable (HE.foldableArray (HE.nullable toPostgres)))

----------------------------------------------------------------------------
-- FromPostgresColumn
----------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} FromPostgres a => FromPostgresColumn a where
  fromPostgresColumn = HD.column (HD.nonNullable fromPostgres)

instance FromPostgres a => FromPostgresColumn (Maybe a) where
  fromPostgresColumn = HD.column (HD.nullable fromPostgres)

instance FromPostgres a => FromPostgresColumn [a] where
  fromPostgresColumn =
    HD.column (HD.nonNullable (HD.listArray (HD.nonNullable fromPostgres)))

instance FromPostgres a => FromPostgresColumn [Maybe a] where
  fromPostgresColumn =
    HD.column (HD.nonNullable (HD.listArray (HD.nullable fromPostgres)))

instance (Ord a, FromPostgres a) => FromPostgresColumn (Set a) where
  fromPostgresColumn =
    Set.fromList <$>
    HD.column (HD.nonNullable (HD.listArray (HD.nonNullable fromPostgres)))

instance (Ord a, FromPostgres a) => FromPostgresColumn (Set (Maybe a)) where
  fromPostgresColumn =
    Set.fromList <$>
    HD.column (HD.nonNullable (HD.listArray (HD.nullable fromPostgres)))

----------------------------------------------------------------------------
-- GToPostgresParams
----------------------------------------------------------------------------

class GToPostgresParams a where
  genericToPostgresParams :: HE.Params a

instance GToPostgresParams () where
  genericToPostgresParams = HE.noParams

instance (ToPostgresParam a, GToPostgresParams b) => GToPostgresParams (a, b) where
  genericToPostgresParams = divided toPostgresParam genericToPostgresParams

-- We only support generics for one-constructor types
instance GToPostgresParams a => GToPostgresParams (Either a Void) where
  genericToPostgresParams = chosen genericToPostgresParams lost

----------------------------------------------------------------------------
-- GFromPostgresRow
----------------------------------------------------------------------------

class GFromPostgresRow a where
  genericFromPostgresRow :: HD.Row a

instance GFromPostgresRow () where
  genericFromPostgresRow = pure ()

instance (FromPostgresColumn a, GFromPostgresRow b) => GFromPostgresRow (a, b) where
  genericFromPostgresRow = (,) <$> fromPostgresColumn <*> genericFromPostgresRow

-- We only support generics for one-constructor types
instance GFromPostgresRow a => GFromPostgresRow (Either a Void) where
  genericFromPostgresRow = Left <$> genericFromPostgresRow

----------------------------------------------------------------------------
-- ToPostgres and FromPostgres instances
----------------------------------------------------------------------------

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
-- Tuples
----------------------------------------------------------------------------

-- Note: 'Generic' provides instances for tuples only until 7-tuples

instance ToPostgresParams ()
instance ToPostgresParam a => ToPostgresParams (Identity a)
instance (ToPostgresParam a, ToPostgresParam b) => ToPostgresParams (a, b)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c) => ToPostgresParams (a, b, c)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d) => ToPostgresParams (a, b, c, d)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d, ToPostgresParam e) => ToPostgresParams (a, b, c, d, e)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d, ToPostgresParam e, ToPostgresParam f) => ToPostgresParams (a, b, c, d, e, f)
instance (ToPostgresParam a, ToPostgresParam b, ToPostgresParam c, ToPostgresParam d, ToPostgresParam e, ToPostgresParam f, ToPostgresParam g) => ToPostgresParams (a, b, c, d, e, f, g)

instance FromPostgresRow ()
instance FromPostgresColumn a => FromPostgresRow (Identity a)
instance (FromPostgresColumn a, FromPostgresColumn b) => FromPostgresRow (a, b)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c) => FromPostgresRow (a, b, c)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d) => FromPostgresRow (a, b, c, d)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d, FromPostgresColumn e) => FromPostgresRow (a, b, c, d, e)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d, FromPostgresColumn e, FromPostgresColumn f) => FromPostgresRow (a, b, c, d, e, f)
instance (FromPostgresColumn a, FromPostgresColumn b, FromPostgresColumn c, FromPostgresColumn d, FromPostgresColumn e, FromPostgresColumn f, FromPostgresColumn g) => FromPostgresRow (a, b, c, d, e, f, g)
