{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Guide.Db.Get where

import Imports

import Contravariant.Extras.Contrazip (contrazip3)
import Hasql.Session (Session)
import Hasql.Statement (Statement (..))
import Named
import Text.RawString.QQ

import qualified Data.Text as T
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as HS

import Guide.Db.Connection (connect, run')
import Guide.Markdown (toMarkdownBlock, toMarkdownInline, toMarkdownTree)
import Guide.Types.Core (Category (..), Item (..), Trait (..))
import Guide.Utils (Uid (..))


-- |
getTest :: IO ()
getTest = do
  conn <- connect
  trait <- run' (getTraitByItemIdMaybe "items1234567" (#deleted False) Pro) conn
  print trait

-- | Methods to get information from database

-- | Get trait by id, either it deleted or not.
getTraitByTraitIdMaybe :: Uid Trait -> Session (Maybe Trait)
getTraitByTraitIdMaybe (Uid uidTrait) = do
  let sql = [r|SELECT uid, content FROM traits WHERE uid = $1;|]
      encoder = textParamNonNull
      decoder = HD.rowMaybe traitRow
  HS.statement uidTrait (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted or not and trat taip.
getTraitByItemIdMaybe :: Uid Item -> "deleted" :! Bool -> TraitType -> Session [Trait]
getTraitByItemIdMaybe (Uid uidItem) (arg #deleted -> deleted) traitType = do
  let sql = [r|
        SELECT uid, content FROM traits
          WHERE item_uid = $1 AND
                deleted = $2 AND
                type_ = ($3 :: trait_type)
        ;|]
      encoder = contrazip3 textParamNonNull boolParamNonNull traitTypeParamNonNull
      decoder = HD.rowList traitRow
  HS.statement (uidItem,deleted,traitType) (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItemByItemIdMaybe :: Uid Item -> Session (Maybe Item)
getItemByItemIdMaybe uid@(Uid uidItem) = do
  pro <- getTraitByItemIdMaybe uid (#deleted False) Pro
  proDeleted <- getTraitByItemIdMaybe uid (#deleted True) Pro
  con <- getTraitByItemIdMaybe uid (#deleted False) Con
  conDeleted <- getTraitByItemIdMaybe uid (#deleted True) Con
  let pref = "item-notes-" <> uidItem <> "-"
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
  HS.statement uidItem (Statement sql encoder decoder False)


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

-- | Trait row
traitRow :: HD.Row Trait
traitRow = do
  _traitUid <- Uid <$> textRowNonNull
  _traitContent <- toMarkdownInline <$> textRowNonNull
  pure $ Trait{..}

-- | Just ADT for traitType
data TraitType = Pro | Con
  deriving Eq

-- | Show instance for TraitType
instance Show TraitType where
  show Pro = "pro"
  show Con = "con"
