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
import Text.RawString.QQ (r)

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
  mTrait <- run' (getTraitMaybe "qwertassdf34") conn
  print mTrait
  traits <- run' (getTraitsByItem "items1234567" (#deleted False) Pro) conn
  print traits
  mItem <- run' (getItemMaybe "items1234567") conn
  print mItem
  item <- run' (getItem "items1234567") conn
  print item
  -- wrong uid
  -- itemErr <- run' (getItemByItemId "wrong1234567") conn
  -- print itemErr
  items <- run' (getItemsByCategory "categories11" (#deleted False)) conn
  print items
  catM <- run' (getCategoryMaybe "categories11") conn
  print catM
  cat <- run' (getCategory "categories11") conn
  print cat
  catId <- run' (getCategoryIdByItem "items1234567") conn
  print catId
  catIds <- run' getCategoryIds conn
  print catIds
  cats <- run' getCategories conn
  print cats

--
-- | Methods to get information from database
--

--
-- | Traits
--

-- | Get trait by id, either it deleted or not.
getTraitMaybe :: Uid Trait -> Session (Maybe Trait)
getTraitMaybe Uid{..} = do
  let sql = [r|SELECT uid, content FROM traits WHERE uid = $1;|]
      encoder = textEncoderNonNull
      decoder = HD.rowMaybe traitRow
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted and trait_type filters.
getTraitsByItem :: Uid Item -> "deleted" :! Bool -> TraitType -> Session [Trait]
getTraitsByItem Uid{..} (arg #deleted -> deleted) traitType = do
  let sql = [r|
        SELECT uid, content FROM traits
          WHERE item_uid = $1 AND
                deleted = $2 AND
                type_ = ($3 :: trait_type)
        ;|]
      encoder = contrazip3 textEncoderNonNull boolEncoderNonNull traitTypeEncoderNonNull
      decoder = HD.rowList traitRow
  HS.statement (uidToText,deleted,traitType) (Statement sql encoder decoder False)

--
-- | Items
--

-- | Get maybe item by id, either it deleted or not.
getItemMaybe :: Uid Item -> Session (Maybe Item)
getItemMaybe uid@Uid{..} = do
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
getItem :: Uid Item -> Session Item
getItem uid@Uid{..} = do
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
getItemsByCategory :: Uid Category -> "deleted" :! Bool -> Session [Item]
getItemsByCategory Uid{..} (arg #deleted -> deleted) = do
  let sql = [r|
        SELECT uid
          from items
          WHERE category_uid = $1 AND
                deleted = $2
        ;|]
      encoder = contrazip2 textEncoderNonNull boolEncoderNonNull
      decoder = HD.rowList $ Uid <$> textDecoderNonNull
  itemUids <- HS.statement (uidToText,deleted) (Statement sql encoder decoder False)
  traverse getItem itemUids

--
-- | Categories
--

-- | Get maybe category by uid.
getCategoryMaybe :: Uid Category -> Session (Maybe Category)
getCategoryMaybe catId@Uid{..} = do
  (_categoryItems, _categoryItemsDeleted) <- categoryItems catId
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
          FROM categories
          WHERE uid = $1
        ;|]
      encoder = textEncoderNonNull
      decoder = HD.rowMaybe $ categoryRow _categoryItems _categoryItemsDeleted
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get category by uid.
getCategory :: Uid Category -> Session Category
getCategory catId@Uid{..} = do
  (_categoryItems, _categoryItemsDeleted) <- categoryItems catId
  let sql = [r|
        SELECT uid, title, created, group_, status_, notes, enabled_sections
          FROM categories
          WHERE uid = $1
        ;|]
      encoder = textEncoderNonNull
      decoder = HD.singleRow $ categoryRow _categoryItems _categoryItemsDeleted
  HS.statement uidToText (Statement sql encoder decoder False)

-- | Get category uid by item uid.
getCategoryIdByItem :: Uid Item -> Session (Uid Category)
getCategoryIdByItem (Uid itemId) = do
  let sql = [r|
        SELECT category_uid
          from items
          WHERE uid = $1
        ;|]
      encoder = textEncoderNonNull
      decoder = HD.singleRow $ Uid <$> textDecoderNonNull
  HS.statement itemId (Statement sql encoder decoder False)

-- | Get category by item uid.
getCategoryByItem :: Uid Item -> Session (Maybe Category)
getCategoryByItem itemId = do
  catId <- getCategoryIdByItem itemId
  getCategoryMaybe catId

-- | Get category's uid list
getCategoryIds :: Session [Uid Category]
getCategoryIds = do
  let sql = [r|
        SELECT uid
          FROM categories
        ;|]
      encoder = HE.noParams
      decoder = HD.rowList $ Uid <$> textDecoderNonNull
  HS.statement () (Statement sql encoder decoder False)

-- | Get all categories
getCategories :: Session [Category]
getCategories = do
  catIds <- getCategoryIds
  traverse getCategory catIds

--
-- | General functions
--

--
-- | Encoders
--

-- | Encoder parameter for text
textEncoderNonNull :: HE.Params Text
textEncoderNonNull = HE.param (HE.nonNullable HE.text)

-- | Encoder parameter for text
traitTypeEncoderNonNull :: HE.Params TraitType
traitTypeEncoderNonNull = HE.param (HE.nonNullable $ HE.enum (T.pack . show))

-- | Encoder parameter for bool
boolEncoderNonNull :: HE.Params Bool
boolEncoderNonNull = HE.param (HE.nonNullable HE.bool)

--
-- | Decoders
--

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

--
-- | Rows
--

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

-- | Category row
categoryRow :: [Item] -> [Item] -> HD.Row Category
categoryRow _categoryItems _categoryItemsDeleted = do
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

--
-- | Other
--

-- | Get all item's traits
itemTraits :: Uid Item -> Session ([Trait], [Trait], [Trait], [Trait])
itemTraits uid = do
  _itemPros <- getTraitsByItem uid (#deleted False) Pro
  _itemProsDeleted <- getTraitsByItem uid (#deleted True) Pro
  _itemCons <- getTraitsByItem uid (#deleted False) Con
  _itemConsDeleted <- getTraitsByItem uid (#deleted True) Con
  pure (_itemPros, _itemProsDeleted, _itemCons, _itemConsDeleted)

-- | Get all category's items
categoryItems :: Uid Category -> Session ([Item], [Item])
categoryItems catId = do
  _categoryItems <- getItemsByCategory catId (#deleted False)
  _categoryItemsDeleted <- getItemsByCategory catId (#deleted True)
  pure (_categoryItems, _categoryItemsDeleted)

-- | Just ADT for traitType
data TraitType = Pro | Con
  deriving Eq

-- | Show instance for TraitType
instance Show TraitType where
  show Pro = "pro"
  show Con = "con"
