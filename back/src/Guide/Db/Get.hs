{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Guide.Db.Get where

import Imports

import Hasql.Session (Session)
import NeatInterpolation
import Hasql.Connection (Connection, Settings)
import Hasql.Statement (Statement (..))

import qualified Hasql.Session as HS
import qualified Hasql.Connection as HC
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

import Guide.Db.Connection (connect, run')
import Guide.Types.Core (Trait(..), Item(..), Category)
import Guide.Markdown (toMarkdownInline, toMarkdownBlock, toMarkdownTree)
import Guide.Utils (Uid(..))


-- |
getTest :: IO ()
getTest = do
  conn <- connect
  trait <- run' (getItemByItemIdMaybe "items1234567") conn
  print trait

-- | Methods to get information from database

-- | Get trait by id, either it deleted or not.
getTraitByTraitIdMaybe :: Uid Trait -> Session (Maybe Trait)
getTraitByTraitIdMaybe (Uid uidTrait) = do
  let sql = toByteString [text|
        SELECT uid, content FROM traits WHERE uid = '${uidTrait}';|]
      encoder = HE.noParams
      decoder = HD.rowMaybe (Trait
        <$> (Uid <$> (HD.column . HD.nonNullable) HD.text)
        <*> (toMarkdownInline <$> (HD.column . HD.nonNullable) HD.text))
  HS.statement () (Statement sql encoder decoder False)

-- | Get traits list by item id, deleted or not and trat taip.
getTraitByItemIdMaybe :: Bool -> Text -> Uid Item -> Session [Trait]
getTraitByItemIdMaybe isDeleted traitType (Uid uidItem) = do
  -- TODO: ProCon datatype to pass here not a text
  let deletedT = toText $ show isDeleted
  let sql = toByteString [text|
        SELECT uid, content FROM traits
          WHERE item_uid = '${uidItem}' AND
                deleted = '${deletedT}' AND
                type_ = '${traitType}'
        ;|]
      encoder = HE.noParams
      decoder = HD.rowList (Trait
        <$> (Uid <$> (HD.column . HD.nonNullable) HD.text)
        <*> (toMarkdownInline <$> (HD.column . HD.nonNullable) HD.text))
  HS.statement () (Statement sql encoder decoder False)

-- | Get item by id, either it deleted or not.
getItemByItemIdMaybe :: Uid Item -> Session (Maybe Item)
getItemByItemIdMaybe uid@(Uid uidItem) = do
  pro <- getTraitByItemIdMaybe False "pro" uid
  proDeleted <- getTraitByItemIdMaybe True "pro" uid
  con <- getTraitByItemIdMaybe False "con" uid
  conDeleted <- getTraitByItemIdMaybe True "con" uid
  let sql = toByteString [text|
        SELECT * FROM items WHERE uid = '${uidItem}';|]
      encoder = HE.noParams
      decoder = HD.rowMaybe (Item
        <$> (Uid <$> (HD.column . HD.nonNullable) HD.text)                             -- uid
        <*> ((HD.column . HD.nonNullable) HD.text)                                     -- name
        <*> (localTimeToUTC utc <$> (HD.column . HD.nonNullable) HD.timestamp)         -- localtime
        <*> ((HD.column . HD.nullable) HD.text)                                        -- group_
        <*> ((HD.column . HD.nullable) HD.text)                                        -- link
        <*> ((HD.column . HD.nullable) HD.text)                                        -- hackage
        <*> (toMarkdownBlock <$> (HD.column . HD.nonNullable) HD.text)                 -- summary
        <*> (toMarkdownBlock <$> (HD.column . HD.nonNullable) HD.text)                 -- ecosystem
        <*> (toMarkdownTree "whatisidprefix" <$> (HD.column . HD.nonNullable) HD.text) -- notes. What a prefix?
        <*> pure pro
        <*> pure proDeleted
        <*> pure con
        <*> pure conDeleted
        )
  HS.statement () (Statement sql encoder decoder False)

