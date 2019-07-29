{-# LANGUAGE OverloadedStrings #-}


-- | Encoders and decoders for types used in the database schema.
module Guide.Database.Convert
       (
       -- * 'Bool'
         boolParam
       , boolParamNullable

       -- * 'Text'
       , textParam
       , textParamNullable
       , textColumn
       , textColumnNullable

       -- * 'Uid'
       , uidParam
       , uidColumn

       -- * 'UTCTime'
       , timestamptzParam
       , timestamptzColumn

       -- * 'TraitType'
       , traitTypeParam

       -- * 'CategoryStatus'
       , categoryStatusParam
       , categoryStatusColumn

       -- * @Set 'ItemSection'@
       , itemSectionSetParam
       , itemSectionSetColumn
       ) where


import Imports

import Data.Functor.Contravariant (contramap)
import qualified Data.Set as Set

import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import Guide.Types.Core (CategoryStatus (..), ItemSection (..), TraitType (..))
import Guide.Utils (Uid (..))


----------------------------------------------------------------------------
-- Bool
----------------------------------------------------------------------------

-- | Pass a 'Bool' to a query.
boolParam :: HE.Params Bool
boolParam = HE.param (HE.nonNullable HE.bool)

-- | Pass a nullable 'Bool' to a query.
boolParamNullable :: HE.Params (Maybe Bool)
boolParamNullable = HE.param (HE.nullable HE.bool)

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

-- | Pass a 'Text' to a query.
textParam :: HE.Params Text
textParam = HE.param (HE.nonNullable HE.text)

-- | Pass a nullable 'Text' to a query.
textParamNullable :: HE.Params (Maybe Text)
textParamNullable = HE.param (HE.nullable HE.text)

-- | Get a 'Text' from a query.
textColumn :: HD.Row Text
textColumn = HD.column (HD.nonNullable HD.text)

-- | Get a nullable 'Text' from a query.
textColumnNullable :: HD.Row (Maybe Text)
textColumnNullable = HD.column (HD.nullable HD.text)

----------------------------------------------------------------------------
-- Uid
----------------------------------------------------------------------------

-- | Pass a 'Uid' to a query.
uidParam :: HE.Params (Uid a)
uidParam = contramap uidToText textParam

-- | Get a 'Uid' from a query.
uidColumn :: HD.Row (Uid a)
uidColumn = Uid <$> textColumn

----------------------------------------------------------------------------
-- UTCTime
----------------------------------------------------------------------------

-- | Encode a 'UTCTime'.
timestamptzParam :: HE.Params UTCTime
timestamptzParam = HE.param (HE.nonNullable HE.timestamptz)

-- | Get a 'UTCTime' from a query.
timestamptzColumn :: HD.Row UTCTime
timestamptzColumn = HD.column (HD.nonNullable HD.timestamptz)

----------------------------------------------------------------------------
-- TraitType
----------------------------------------------------------------------------

-- | Encode a 'TraitType'.
traitTypeEncoder :: HE.Value TraitType
traitTypeEncoder = HE.enum $ \case
  Pro -> "pro"
  Con -> "con"

-- | Pass a 'TraitType' to a query.
traitTypeParam :: HE.Params TraitType
traitTypeParam = HE.param (HE.nonNullable traitTypeEncoder)

----------------------------------------------------------------------------
-- CategoryStatus
----------------------------------------------------------------------------

-- | Encode a 'CategoryStatus'.
categoryStatusEncoder :: HE.Value CategoryStatus
categoryStatusEncoder = HE.enum $ \case
  CategoryStub -> "stub"
  CategoryWIP -> "wip"
  CategoryFinished -> "finished"

-- | Pass a 'CategoryStatus' to a query.
categoryStatusParam :: HE.Params CategoryStatus
categoryStatusParam = HE.param (HE.nonNullable categoryStatusEncoder)

-- | Decode a 'CategoryStatus'.
categoryStatusDecoder :: HD.Value CategoryStatus
categoryStatusDecoder = HD.enum $ \case
  "stub" -> Just CategoryStub
  "wip" -> Just CategoryWIP
  "finished" -> Just CategoryFinished
  _ -> Nothing

-- | Get a 'CategoryStatus' from a query.
categoryStatusColumn :: HD.Row CategoryStatus
categoryStatusColumn = HD.column (HD.nonNullable categoryStatusDecoder)

----------------------------------------------------------------------------
-- Set ItemSection
----------------------------------------------------------------------------

-- | Encode an 'ItemSection'.
itemSectionEncoder :: HE.Value ItemSection
itemSectionEncoder = HE.enum $ \case
  ItemProsConsSection -> "pros_cons"
  ItemEcosystemSection -> "ecosystem"
  ItemNotesSection -> "notes"

-- | Pass a @Set ItemSection@ to a query.
itemSectionSetParam :: HE.Params (Set ItemSection)
itemSectionSetParam = contramap Set.toList
  $ HE.param
  $ HE.nonNullable
  $ HE.foldableArray
  $ HE.nonNullable
  $ itemSectionEncoder

-- | Decode an 'ItemSection'.
itemSectionDecoder :: HD.Value ItemSection
itemSectionDecoder = HD.enum $ \case
  "pros_cons" -> Just ItemProsConsSection
  "ecosystem" -> Just ItemEcosystemSection
  "notes" -> Just ItemNotesSection
  _ -> Nothing

-- | Get a @Set ItemSection@ from a query.
itemSectionSetColumn :: HD.Row (Set ItemSection)
itemSectionSetColumn =
  fmap Set.fromList
  $ HD.column
  $ HD.nonNullable
  $ HD.listArray
  $ HD.nonNullable
  $ itemSectionDecoder
