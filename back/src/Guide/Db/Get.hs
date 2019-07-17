{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Guide.Db.Get where

import Imports

import Contravariant.Extras.Contrazip (contrazip2, contrazip3)
import Hasql.Session (Session)
import Hasql.Statement (Statement (..))
import Named
import Text.RawString.QQ

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS

import Guide.Db.Connection (connect, run')
import Guide.Markdown (toMarkdownBlock, toMarkdownInline, toMarkdownTree)
import Guide.Types.Core (Category (..), Item (..), Trait (..), toSection, toStatus)
import Guide.Utils (Uid (..))


-- |
getTest :: IO ()
getTest = do
  conn <- connect
  trait <- run' (getTraitsByItemId "items1234567" (#deleted False) Pro) conn
  print trait

-- | Methods to get information from database

-- | Get trait by id, either it deleted or not.
getTraitByTraitIdMaybe :: Uid Trait -> Session (Maybe Trait)
getTraitByTraitIdMaybe Uid{..} = do
  let sql = [r|SELECT uid, content FROM traits WHERE uid = $1;|]
      encoder = textParamNonNull
      decoder = HD.rowMaybe traitRow
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted and trait_type filters.
getTraitsByItemId :: Uid Item -> "deleted" :! Bool -> TraitType -> Session [Trait]
getTraitsByItemId Uid{..} (arg #deleted -> deleted) traitType = do
  let sql = [r|
        SELECT uid, content FROM traits
          WHERE item_uid = $1 AND
                deleted = $2 AND
                type_ = ($3 :: trait_type)
        ;|]
      encoder = contrazip3 textParamNonNull boolParamNonNull traitTypeParamNonNull
      decoder = HD.rowList traitRow
  HS.statement (uidToText,deleted,traitType) (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItemByItemIdMaybe :: Uid Item -> Session (Maybe Item)
getItemByItemIdMaybe uid@Uid{..} = do
  pro <- getTraitsByItemId uid (#deleted False) Pro
  proDeleted <- getTraitsByItemId uid (#deleted True) Pro
  con <- getTraitsByItemId uid (#deleted False) Con
  conDeleted <- getTraitsByItemId uid (#deleted True) Con
  let pref = "item-notes-" <> uidToText <> "-"
  let sql = [r|
        SELECT uid, name, created, group_, link, hackage, summary, ecosystem, note
          FROM items
          WHERE uid = $1;
        |]
      encoder = textParamNonNull
      decoder = HD.rowMaybe $ do
        _itemUid <- Uid <$> textRowNonNull
        _itemName <- textRowNonNull
        _itemCreated <- localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp
        _itemGroup_ <- textRowNull
        _itemLink <- textRowNull
        _itemHackage <- textRowNull
        _itemSummary <- toMarkdownBlock <$> textRowNonNull
        _itemEcosystem <- toMarkdownBlock <$> textRowNonNull
        _itemNotes <- toMarkdownTree pref <$> textRowNonNull
        let _itemPros = pro
        let _itemProsDeleted = proDeleted
        let _itemCons = con
        let _itemConsDeleted = conDeleted
        pure $ Item{..}
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get items list by category id and deleted filters
getItemsByCatId :: Uid Category -> "deleted" :! Bool -> Session [Item]
getItemsByCatId Uid{..} (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid, name, created, group_, link, hackage, summary, ecosystem, note
          FROM items
          WHERE category_uid = $1 AND
                deleted = $2 AND
        ;|]
      encoder = contrazip2 textParamNonNull boolParamNonNull
      decoder = HD.rowList $ do
        uid <- textRowNonNull
        let _itemUid = Uid uid
        let pref = "item-notes-" <> uid <> "-"
        _itemName <- textRowNonNull
        _itemCreated <- localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp
        _itemGroup_ <- textRowNull
        _itemLink <- textRowNull
        _itemHackage <- textRowNull
        _itemSummary <- toMarkdownBlock <$> textRowNonNull
        _itemEcosystem <- toMarkdownBlock <$> textRowNonNull
        _itemNotes <- toMarkdownTree pref <$> textRowNonNull

        -- How-to get procon with item uid fetched just now?

        -- let _itemPros = pro
        -- let _itemProsDeleted = proDeleted
        -- let _itemCons = con
        -- let _itemConsDeleted = conDeleted
        pure $ Item{..}
  HS.statement (uidToText,deleted) (Statement sql encoder decoder False)

-- | Get category by id, either it deleted or not.
getCatByCatIdMaybe :: Uid Category -> Session (Maybe Category)
getCatByCatIdMaybe Uid{..} = do
  -- _categoryItems <- getItemByCatIdMaybe uid (#deleted False)
  -- _categoryItemsDeleted <- getItemBycatIdMaybe uid (#deleted True)
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
          FROM categories
          WHERE uid = $1;
        |]
      encoder = textParamNonNull
      decoder = HD.rowMaybe $ do
        _categoryUid <- Uid <$> textRowNonNull
        _categoryTitle <- textRowNonNull
        _categoryCreated <- localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp
        _categoryGroup_ <- textRowNonNull
        _categoryStatus <- toStatus <$> textRowNonNull
        _categoryNotes <- toMarkdownBlock <$> textRowNonNull
        selectionsText <- textArrayDecoder
        let _categoryEnabledSections = Set.fromList $ map toSection selectionsText
        -- let _itemPros = pro
        -- let _itemProsDeleted = proDeleted
        let _categoryGroups = Map.empty
        pure $ Category{..}
  HS.statement uidToText (Statement sql encoder decoder False)


-- | General functions

-- | Encoder parameter for text
textParamNonNull :: HE.Params Text
textParamNonNull = HE.param (HE.nonNullable HE.text)

-- | Encoder parameter for text
traitTypeParamNonNull :: HE.Params TraitType
traitTypeParamNonNull = HE.param (HE.nonNullable $ HE.enum (T.pack . show))

-- | Encoder parameter for bool
boolParamNonNull :: HE.Params Bool
boolParamNonNull = HE.param (HE.nonNullable HE.bool)

-- | Decoder row for nonNull text
textRowNonNull :: HD.Row Text
textRowNonNull = (HD.column . HD.nonNullable) HD.text

-- | Decoder row for maybe text
textRowNull :: HD.Row (Maybe Text)
textRowNull = (HD.column . HD.nullable) HD.text

-- | Decoder for [text]
textArrayDecoder :: HD.Row [Text]
textArrayDecoder = (HD.column . HD.nonNullable)
  $ HD.array (HD.dimension replicateM (HD.element $ HD.nonNullable HD.text))

-- | Trait row
traitRow :: HD.Row Trait
traitRow = do
  _traitUid <- Uid <$> textRowNonNull
  _traitContent <- toMarkdownInline <$> textRowNonNull
  pure $ Trait{..}

-- | Trait row
-- itemRow :: HD.Row Item
-- itemRow = do
--   uid <- textRowNonNull
--   let _itemUid = Uid uid
--   let pref = "item-notes-" <> uid <> "-"
--   _itemName <- textRowNonNull
--   _itemCreated <- localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp
--   _itemGroup_ <- textRowNull
--   _itemLink <- textRowNull
--   _itemHackage <- textRowNull
--   _itemSummary <- toMarkdownBlock <$> textRowNonNull
--   _itemEcosystem <- toMarkdownBlock <$> textRowNonNull
--   _itemNotes <- toMarkdownTree pref <$> textRowNonNull
--   pure $ Item{..}

-- | Just ADT for traitType
data TraitType = Pro | Con
  deriving Eq

-- | Show instance for TraitType
instance Show TraitType where
  show Pro = "pro"
  show Con = "con"
