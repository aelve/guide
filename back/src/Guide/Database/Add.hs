{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Insert database queries.
module Guide.Database.Add
       (
       -- * Trait
         addTrait
       -- * Item
       , addItem
       -- * Category
       , addCategory

       ) where

import Imports

import Contravariant.Extras.Contrazip (contrazip4, contrazip7, contrazip9)
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Text.RawString.QQ (r)

import qualified Data.Set as Set
import qualified Hasql.Decoders as HD
import qualified Hasql.Transaction as HT

import Guide.Database.Convert
import Guide.Database.Types
import Guide.Types.Core (Category (..), CategoryStatus (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


-- Insert category to database
addCategory
  :: Uid Category    -- ^ New category's id
  -> Text            -- ^ Title
  -> Text            -- ^ Group
  -> UTCTime         -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addCategory catId title group_ created = do
  let sql = [r|
        INSERT INTO categories (uid, title, created, group_, status_, notes, enabled_sections)
        VALUES ($1,$2,$3,$4,$5,$6,$7)
        |]
      encoder = contrazip7
        uidParam
        textParam
        timestamptzParam
        textParam
        categoryStatusParam
        textParam
        itemSectionSetParam
      decoder = HD.noResult
  lift $ HT.statement (catId, title, created, group_, CategoryWIP, "", Set.empty)
    (Statement sql encoder decoder False)

-- Insert item to database
addItem
  :: Uid Category    -- ^ Category id
  -> Uid Item        -- ^ New item's id
  -> Text            -- ^ Name
  -> UTCTime         -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addItem catId itemId name created = do
  let sql = [r|
        INSERT INTO items (uid, name, created, link, hackage, summary, ecosystem, notes, category_uid)
        VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)
        |]
      encoder = contrazip9
        uidParam
        textParam
        timestamptzParam
        textParamNullable
        textParamNullable
        textParam
        textParam
        textParam
        uidParam
      decoder = HD.noResult
  lift $ HT.statement (itemId, name, created, Nothing, Nothing, "", "", "", catId)
    (Statement sql encoder decoder False)

-- Insert trait to database
addTrait
  :: Uid Item        -- ^ Item id
  -> Uid Trait       -- ^ New trait's id
  -> TraitType       -- ^ Pro or Con
  -> Text            -- ^ Trait content
  -> ExceptT DatabaseError Transaction ()
addTrait itemId traitId type_ content = do
  let sql = [r|
        INSERT INTO items (uid, content, type_, item_uid)
        VALUES ($1,$2,$3,$4)
        |]
      encoder = contrazip4 uidParam textParam traitTypeParam uidParam
      decoder = HD.noResult
  lift $ HT.statement (traitId, content, type_, itemId) (Statement sql encoder decoder False)
