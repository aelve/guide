{-# LANGUAGE OverloadedStrings #-}


-- | Convert function used to combine hasql queries.
module Guide.Database.Convert
       (
       -- * Bool
         boolParam
       , boolParamNullable
       -- * Text
       , textParam
       , textColumn
       , textParamNullable
       , textColumnNullable
       -- * TraitType
       , traitTypeParam
       -- * [Text]
       , textArrayColumn
       -- * Uid
       , uidParam
       , uidColumn
       -- * UTCTime
       , timestamptzColumn
       -- * ItemSection
       , itemSectionColumn
       -- * CategoryStatus
       , categoryStatusColumn
       ) where


import Imports

import Data.Functor.Contravariant (contramap)

import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import Guide.Types.Core (CategoryStatus (..), ItemSection (..), TraitType (..))
import Guide.Utils (Uid (..))


-- Convert functions for database

----------------------------------------------------------------------------
-- Bool
----------------------------------------------------------------------------

-- | nonNullable Bool
boolEncoder :: HE.NullableOrNot HE.Value Bool
boolEncoder = HE.nonNullable HE.bool

-- | nullable Bool
boolEncoderNullable :: HE.NullableOrNot HE.Value (Maybe Bool)
boolEncoderNullable = HE.nullable HE.bool

-- | Encode bool
boolParam :: HE.Params Bool
boolParam = HE.param boolEncoder

-- | Encode nullable bool
boolParamNullable :: HE.Params (Maybe Bool)
boolParamNullable = HE.param boolEncoderNullable

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

-- | nonNullable Text for Encoder
textEncoder :: HE.NullableOrNot HE.Value Text
textEncoder = HE.nonNullable HE.text

-- | nonNullable Text for Decoder
textDecoder :: HD.NullableOrNot HD.Value Text
textDecoder = HD.nonNullable HD.text

-- | nullable Text for Encoder
textEncoderNullable :: HE.NullableOrNot HE.Value (Maybe Text)
textEncoderNullable = HE.nullable HE.text

-- | nullable Text for Decoder
textDecoderNullable :: HD.NullableOrNot HD.Value (Maybe Text)
textDecoderNullable = HD.nullable HD.text

-- | Encode text
textParam :: HE.Params Text
textParam = HE.param textEncoder

-- | Decode text
textColumn :: HD.Row Text
textColumn = HD.column textDecoder

-- | Encode nullable text
textParamNullable :: HE.Params (Maybe Text)
textParamNullable = HE.param textEncoderNullable

-- | Decode nullable text
textColumnNullable :: HD.Row (Maybe Text)
textColumnNullable = HD.column textDecoderNullable

----------------------------------------------------------------------------
-- TraitType
----------------------------------------------------------------------------

-- | Encode TraitType
traitTypeParam :: HE.Params TraitType
traitTypeParam = HE.param (HE.nonNullable $ HE.enum showTraitType)
  where
    showTraitType = \case
      Pro -> "pro"
      Con -> "con"

----------------------------------------------------------------------------
-- Uid
----------------------------------------------------------------------------

-- | Encode Uid
uidParam :: HE.Params (Uid a)
uidParam = contramap uidToText textParam

-- | Decode Uid
uidColumn :: HD.Row (Uid a)
uidColumn = Uid <$> textColumn

----------------------------------------------------------------------------
-- [Text]
----------------------------------------------------------------------------

-- | Decode [Text]
textArrayColumn :: HD.Row [Text]
textArrayColumn = (HD.column . HD.nonNullable)
  $ HD.array (HD.dimension replicateM (HD.element textDecoder))

----------------------------------------------------------------------------
-- UTCTime
----------------------------------------------------------------------------

-- Decode UTCTime
timestamptzColumn :: HD.Row UTCTime
timestamptzColumn = HD.column (HD.nonNullable HD.timestamptz)

----------------------------------------------------------------------------
-- CategoryStatus
----------------------------------------------------------------------------

-- | Decode CategoryStatus
categoryStatusColumn :: HD.Row CategoryStatus
categoryStatusColumn = HD.column (HD.nonNullable $ HD.enum showCategoryStatus)
  where
    showCategoryStatus = \case
      "stub" -> Just CategoryStub
      "wip" -> Just CategoryWIP
      "finished" -> Just CategoryFinished
      _ -> Nothing

----------------------------------------------------------------------------
-- [ItemSection]
----------------------------------------------------------------------------

-- | Decode [ItemSection]
itemSectionColumn :: HD.Row [ItemSection]
itemSectionColumn = HD.column
    $ HD.nonNullable
    $ HD.listArray
    $ HD.nonNullable
    $ HD.enum showItemSection
  where
    showItemSection = \case
      "ItemProsConsSection" -> Just ItemProsConsSection
      "ItemEcosystemSection" -> Just ItemEcosystemSection
      "ItemNotesSection" -> Just ItemNotesSection
      _ -> Nothing
