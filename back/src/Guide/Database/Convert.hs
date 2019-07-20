{-# LANGUAGE OverloadedStrings #-}


module Guide.Database.Convert
       (
       -- Encoders
         boolParam
       , boolParamNullable
       , textParam
       , textParamNullable
       , traitTypeParam
       , uidParam
       -- Decoders
       , textArrayColumn
       , textDecoderNullable
       , textColumn
       , timestamptzColumn
       , uidColumn
       , categoryRow
       , itemRow
       , traitRow
       ) where


import Imports

import Data.Functor.Contravariant (contramap)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE

import Guide.Markdown (toMarkdownBlock, toMarkdownInline, toMarkdownTree)
import Guide.Types.Core (Category (..), CategoryStatus(..), Item (..), ItemSection(..), Trait (..),
                         TraitType (..))
import Guide.Utils (Uid (..))


-- Convert functions for database

----------------------------------------------------------------------------
-- Encoders
----------------------------------------------------------------------------

-- | nonNullable Bool
boolEncoder :: HE.NullableOrNot HE.Value Bool
boolEncoder = HE.nonNullable HE.bool

-- | nullable Bool
boolEncoderNullable :: HE.NullableOrNot HE.Value (Maybe Bool)
boolEncoderNullable = HE.nullable HE.bool

-- | nonNullable Text
textEncoder :: HE.NullableOrNot HE.Value Text
textEncoder = HE.nonNullable HE.text

-- | nullable Text
textEncoderNullable :: HE.NullableOrNot HE.Value (Maybe Text)
textEncoderNullable = HE.nullable HE.text

-- | Encode bool
boolParam :: HE.Params Bool
boolParam = HE.param boolEncoder

-- | Encode nullable bool
boolParamNullable :: HE.Params (Maybe Bool)
boolParamNullable = HE.param boolEncoderNullable

-- | Encode text
textParam :: HE.Params Text
textParam = HE.param textEncoder

-- | Encode nullable text
textParamNullable :: HE.Params (Maybe Text)
textParamNullable = HE.param textEncoderNullable

-- | Encode TraitType
traitTypeParam :: HE.Params TraitType
traitTypeParam = HE.param (HE.nonNullable $ HE.enum (T.pack . show))

-- | Encode Uid
uidParam :: HE.Params (Uid a)
uidParam = contramap uidToText textParam

----------------------------------------------------------------------------
-- Decoders
----------------------------------------------------------------------------

-- | nonNullable text
textDecoder :: HD.NullableOrNot HD.Value Text
textDecoder = HD.nonNullable HD.text

-- | nullable text
textDecoderNullable :: HD.NullableOrNot HD.Value (Maybe Text)
textDecoderNullable = HD.nullable HD.text

-- | Decode text
textColumn :: HD.Row Text
textColumn = HD.column textDecoder

-- | Decode nullable text
textColumnNullable :: HD.Row (Maybe Text)
textColumnNullable = HD.column textDecoderNullable

-- | Decode [text]
textArrayColumn :: HD.Row [Text]
textArrayColumn = (HD.column . HD.nonNullable)
  $ HD.array (HD.dimension replicateM (HD.element textDecoder))

-- Decode UTCTime
timestamptzColumn :: HD.Row UTCTime
timestamptzColumn = HD.column (HD.nonNullable HD.timestamptz)

-- Decode Uid
uidColumn :: HD.Row (Uid a)
uidColumn = Uid <$> textColumn

-- | Decode Trait
traitRow :: HD.Row Trait
traitRow = do
  _traitUid <- uidColumn
  _traitContent <- toMarkdownInline <$> textColumn
  pure $ Trait{..}

-- | Decode Item
itemRow :: Text -> [Trait] -> [Trait] -> [Trait] -> [Trait] -> HD.Row Item
itemRow pref _itemPros _itemProsDeleted _itemCons _itemConsDeleted = do
  _itemUid <- uidColumn
  _itemName <- textColumn
  _itemCreated <- timestamptzColumn
  _itemGroup_ <- textColumnNullable
  _itemLink <- textColumnNullable
  _itemHackage <- textColumnNullable
  _itemSummary <- toMarkdownBlock <$> textColumn
  _itemEcosystem <- toMarkdownBlock <$> textColumn
  _itemNotes <- toMarkdownTree pref <$> textColumn
  pure $ Item{..}

-- | Decode Category
categoryRow :: [Item] -> [Item] -> HD.Row Category
categoryRow _categoryItems _categoryItemsDeleted = do
  _categoryUid <- uidColumn
  _categoryTitle <- textColumn
  _categoryCreated <- timestamptzColumn
  _categoryGroup_ <- textColumn
  _categoryStatus <- toStatus <$> textColumn
  _categoryNotes <- toMarkdownBlock <$> textColumn
  selectionsText <- textArrayColumn
  let _categoryEnabledSections = Set.fromList $ map toSection selectionsText
  let _categoryGroups = Map.empty
  pure $ Category{..}

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

-- | Read text to CategoryStatus
toStatus :: Text -> CategoryStatus
toStatus = \case
  "stub" -> CategoryStub
  "wip" -> CategoryWIP
  "finished" -> CategoryFinished
  _ -> error "Unknown CategoryStatus"

-- | Read text to ItemSection
toSection :: Text -> ItemSection
toSection = \case
  "ItemProsConsSection" -> ItemProsConsSection
  "ItemEcosystemSection" -> ItemEcosystemSection
  "ItemNotesSection" -> ItemNotesSection
  _ -> error "Unknown ItemSection"
