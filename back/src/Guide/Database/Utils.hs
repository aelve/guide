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
)
where

import Imports
import Named
import Hasql.Statement
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Contravariant.Divisible (divided, lost, chosen)
import Generics.Eot

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

instance ToPostgres a => ToPostgresParam (Maybe a) where
  toPostgresParam = HE.param (HE.nullable toPostgres)

instance ToPostgres a => ToPostgresParam a where
  toPostgresParam = HE.param (HE.nonNullable toPostgres)

----------------------------------------------------------------------------
-- FromPostgresColumn
----------------------------------------------------------------------------

instance FromPostgres a => FromPostgresColumn (Maybe a) where
  fromPostgresColumn = HD.column (HD.nullable fromPostgres)

instance FromPostgres a => FromPostgresColumn a where
  fromPostgresColumn = HD.column (HD.nonNullable fromPostgres)

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

instance ToPostgres Text where
  toPostgres = HE.text

instance FromPostgres Text where
  fromPostgres = HD.text

instance ToPostgres (Uid a) where
  toPostgres = uidToText >$< HE.text

instance FromPostgres (Uid a) where
  fromPostgres = Uid <$> HD.text

----------------------------------------------------------------------------
-- Tuples
----------------------------------------------------------------------------

-- Note: 'Generic' provides instances for tuples only until 7-tuples

instance (ToPostgres a, ToPostgres b) => ToPostgresParams (a, b)
instance (ToPostgres a, ToPostgres b, ToPostgres c) => ToPostgresParams (a, b, c)
instance (ToPostgres a, ToPostgres b, ToPostgres c, ToPostgres d) => ToPostgresParams (a, b, c, d)
instance (ToPostgres a, ToPostgres b, ToPostgres c, ToPostgres d, ToPostgres e) => ToPostgresParams (a, b, c, d, e)
instance (ToPostgres a, ToPostgres b, ToPostgres c, ToPostgres d, ToPostgres e, ToPostgres f) => ToPostgresParams (a, b, c, d, e, f)
instance (ToPostgres a, ToPostgres b, ToPostgres c, ToPostgres d, ToPostgres e, ToPostgres f, ToPostgres g) => ToPostgresParams (a, b, c, d, e, f, g)

instance (FromPostgres a, FromPostgres b) => FromPostgresRow (a, b)
instance (FromPostgres a, FromPostgres b, FromPostgres c) => FromPostgresRow (a, b, c)
instance (FromPostgres a, FromPostgres b, FromPostgres c, FromPostgres d) => FromPostgresRow (a, b, c, d)
instance (FromPostgres a, FromPostgres b, FromPostgres c, FromPostgres d, FromPostgres e) => FromPostgresRow (a, b, c, d, e)
instance (FromPostgres a, FromPostgres b, FromPostgres c, FromPostgres d, FromPostgres e, FromPostgres f) => FromPostgresRow (a, b, c, d, e, f)
instance (FromPostgres a, FromPostgres b, FromPostgres c, FromPostgres d, FromPostgres e, FromPostgres f, FromPostgres g) => FromPostgresRow (a, b, c, d, e, f, g)
