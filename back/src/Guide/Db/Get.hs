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


-- | Just to test queries
getTest :: IO ()
getTest = do
  conn <- connect
  mTrait <- run' (getTraitByTraitIdMaybe "qwertassdf34") conn
  print mTrait
  traits <- run' (getTraitsByItemId "items1234567" (#deleted False) Pro) conn
  print traits
  mItem <- run' (getItemByItemIdMaybe "items1234567") conn
  print mItem
  item <- run' (getItemByItemId "items1234567") conn
  print item
  -- wrong uid
  -- itemErr <- run' (getItemByItemId "wrong1234567") conn
  -- print itemErr
  items <- run' (getItemsByCatId "categories11" (#deleted False)) conn
  print items
  cat <- run' (getCatByCatIdMaybe "categories11") conn
  print cat

-- | Methods to get information from database

-- | Get trait by id, either it deleted or not.
getTraitByTraitIdMaybe :: Uid Trait -> Session (Maybe Trait)
getTraitByTraitIdMaybe Uid{..} = do
  let sql = [r|SELECT uid, content FROM traits WHERE uid = $1;|]
      encoder = textEncoderNonNull
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
      encoder = contrazip3 textEncoderNonNull boolEncoderNonNull traitTypeEncoderNonNull
      decoder = HD.rowList traitRow
  HS.statement (uidToText,deleted,traitType) (Statement sql encoder decoder False)

-- | Get maybe item by id, either it deleted or not.
getItemByItemIdMaybe :: Uid Item -> Session (Maybe Item)
getItemByItemIdMaybe uid@Uid{..} = do
  (_itemPros, _itemProsDeleted, _itemCons, _itemConsDeleted) <- itemTraits uid
  let pref = "item-notes-" <> uidToText <> "-"
  let sql = [r|
        SELECT uid, name, created, group_, link, hackage, summary, ecosystem, notes
          FROM items
          WHERE uid = $1;
        |]
      encoder = textEncoderNonNull
      decoder = HD.rowMaybe $
        itemRow pref _itemPros _itemProsDeleted _itemCons _itemConsDeleted
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItemByItemId :: Uid Item -> Session Item
getItemByItemId uid@Uid{..} = do
  (_itemPros, _itemProsDeleted, _itemCons, _itemConsDeleted) <- itemTraits uid
  let pref = "item-notes-" <> uidToText <> "-"
  let sql = [r|
        SELECT uid, name, created, group_, link, hackage, summary, ecosystem, notes
          FROM items
          WHERE uid = $1
        ;|]
      encoder = textEncoderNonNull
      decoder = HD.singleRow $
        itemRow pref _itemPros _itemProsDeleted _itemCons _itemConsDeleted
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get items with category id and deleted filters
getItemsByCatId :: Uid Category -> "deleted" :! Bool -> Session [Item]
getItemsByCatId Uid{..} (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid
          from items
          WHERE category_uid = $1 AND
                deleted = $2
        ;|]
      encoder = contrazip2 textEncoderNonNull boolEncoderNonNull
      decoder = HD.rowList $ Uid <$> textDecoderNonNull
  itemUids <- HS.statement (uidToText,deleted) (Statement sql encoder decoder False)
  traverse getItemByItemId itemUids

-- | Get category by id, either it deleted or not.
getCatByCatIdMaybe :: Uid Category -> Session (Maybe Category)
getCatByCatIdMaybe catId@Uid{..} = do
  _categoryItems <- getItemsByCatId catId (#deleted False)
  _categoryItemsDeleted <- getItemsByCatId catId (#deleted True)
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
          FROM categories
          WHERE uid = $1
        ;|]
      encoder = textEncoderNonNull
      decoder = HD.rowMaybe $ do
        _categoryUid <- Uid <$> textDecoderNonNull
        _categoryTitle <- textDecoderNonNull
        _categoryCreated <- localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp
        _categoryGroup_ <- textDecoderNonNull
        _categoryStatus <- toStatus <$> textDecoderNonNull
        _categoryNotes <- toMarkdownBlock <$> textDecoderNonNull
        selectionsText <- textArrayDecoder
        let _categoryEnabledSections = Set.fromList $ map toSection selectionsText
        let _categoryGroups = Map.empty
        pure $ Category{..}
  HS.statement uidToText (Statement sql encoder decoder False)


-- | General functions

-- | Encoder parameter for text
textEncoderNonNull :: HE.Params Text
textEncoderNonNull = HE.param (HE.nonNullable HE.text)

-- | Encoder parameter for text
traitTypeEncoderNonNull :: HE.Params TraitType
traitTypeEncoderNonNull = HE.param (HE.nonNullable $ HE.enum (T.pack . show))

-- | Encoder parameter for bool
boolEncoderNonNull :: HE.Params Bool
boolEncoderNonNull = HE.param (HE.nonNullable HE.bool)

-- | Decoder for nonNull text
textDecoderNonNull :: HD.Row Text
textDecoderNonNull = (HD.column . HD.nonNullable) HD.text

-- | Decoder row for maybe text
textDecoderNull :: HD.Row (Maybe Text)
textDecoderNull = (HD.column . HD.nullable) HD.text

-- | Decoder for [text]
textArrayDecoder :: HD.Row [Text]
textArrayDecoder = (HD.column . HD.nonNullable)
  $ HD.array (HD.dimension replicateM (HD.element $ HD.nonNullable HD.text))

-- | Trait row
traitRow :: HD.Row Trait
traitRow = do
  _traitUid <- Uid <$> textDecoderNonNull
  _traitContent <- toMarkdownInline <$> textDecoderNonNull
  pure $ Trait{..}

-- | Item row
itemRow :: Text -> [Trait] -> [Trait] -> [Trait] -> [Trait] -> HD.Row Item
itemRow pref _itemPros _itemProsDeleted _itemCons _itemConsDeleted = do
  _itemUid <- Uid <$> textDecoderNonNull
  _itemName <- textDecoderNonNull
  _itemCreated <- localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp
  _itemGroup_ <- textDecoderNull
  _itemLink <- textDecoderNull
  _itemHackage <- textDecoderNull
  _itemSummary <- toMarkdownBlock <$> textDecoderNonNull
  _itemEcosystem <- toMarkdownBlock <$> textDecoderNonNull
  _itemNotes <- toMarkdownTree pref <$> textDecoderNonNull
  pure $ Item{..}

-- | Get all item's traits
itemTraits :: Uid Item -> Session ([Trait], [Trait], [Trait], [Trait])
itemTraits uid = do
  _itemPros <- getTraitsByItemId uid (#deleted False) Pro
  _itemProsDeleted <- getTraitsByItemId uid (#deleted True) Pro
  _itemCons <- getTraitsByItemId uid (#deleted False) Con
  _itemConsDeleted <- getTraitsByItemId uid (#deleted True) Con
  pure (_itemPros, _itemProsDeleted, _itemCons, _itemConsDeleted)

-- | Just ADT for traitType
data TraitType = Pro | Con
  deriving Eq

-- | Show instance for TraitType
instance Show TraitType where
  show Pro = "pro"
  show Con = "con"
