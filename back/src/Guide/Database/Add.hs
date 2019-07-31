{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeOperators     #-}

-- | Insert queries.
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

import Contravariant.Extras.Contrazip (contrazip4, contrazip8, contrazip9)
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (Mode (..))
import Named
import Text.RawString.QQ (r)

import qualified Data.Set as Set
import qualified Hasql.Decoders as HD
import qualified Hasql.Transaction as HT

import Guide.Database.Connection (connect, runTransactionExceptT)
import Guide.Database.Convert
import Guide.Database.Get
import Guide.Database.Set (addItemIdToCategory)
import Guide.Database.Types
import Guide.Types.Core (Category (..), CategoryStatus (..), Item (..), Trait (..), TraitType (..))
import Guide.Utils (Uid (..))


-- | Insert category to database.
addCategory
  :: Uid Category       -- ^ New category's id
  -> "title" :! Text    -- ^ Title
  -> "group_" :! Text   -- ^ Group
  -> UTCTime            -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addCategory catId (arg #title -> title) (arg #group_ -> group_) created = do
  let sql = [r|
        INSERT INTO categories (uid, title, created, group_, status_, notes, enabled_sections, items_order)
        VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
        |]
      encoder = contrazip8
        uidParam
        textParam
        timestamptzParam
        textParam
        categoryStatusParam
        textParam
        itemSectionSetParam
        uidsParam
      decoder = HD.noResult
  lift $ HT.statement (catId, title, created, group_, CategoryWIP, "", Set.empty, [])
    (Statement sql encoder decoder False)

-- | Insert item to database.
addItem
  :: Uid Category       -- ^ Category id
  -> Uid Item           -- ^ New item's id
  -> "name" :! Text     -- ^ Name
  -> UTCTime            -- ^ Creation time
  -> ExceptT DatabaseError Transaction ()
addItem catId itemId (arg #name -> name) created = do
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
  -- Adds itemId to category's items_order list.
  addItemIdToCategory catId itemId

-- | Insert trait to database.
addTrait
  :: Uid Item             -- ^ Item id
  -> Uid Trait            -- ^ New trait's id
  -> TraitType            -- ^ Pro or Con
  -> "content" :! Text    -- ^ Trait content
  -> ExceptT DatabaseError Transaction ()
addTrait itemId traitId type_ (arg #content -> content) = do
  let sql = [r|
        INSERT INTO traits (uid, content, type_, item_uid)
        VALUES ($1,$2,($3 :: trait_type),$4)
        |]
      encoder = contrazip4 uidParam textParam traitTypeParam uidParam
      decoder = HD.noResult
  lift $ HT.statement (traitId, content, type_, itemId) (Statement sql encoder decoder False)


-- Sandbox

-- Test add functions
testAdd :: IO ()
testAdd = do
  conn <- connect
  time <- getCurrentTime
  -- runTransactionExceptT conn Write (addCategory "category1111" (#title "addedCat") (#group_ "groupCat") time)
  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print cat

  runTransactionExceptT conn Write (addItem "category1111" "item11112222" (#name "addedItem") time)
  item <- runTransactionExceptT conn Read (getItem "item11112222")
  print item
  -- runTransactionExceptT conn Write (addItem "category1111" "item22223333" (#name "addedItem") time)
  -- item <- runTransactionExceptT conn Read (getItem "item22223333")
  -- print item


  runTransactionExceptT conn Write (addTrait "item11112222" "trait1113333" Pro (#content "content"))
  trait' <- runTransactionExceptT conn Read (getTraitMaybe "trait2223333")
  print trait'
  -- runTransactionExceptT conn Write (addTrait "item22223333" "trait2223333" Con (#content "content22"))
  -- trait <- runTransactionExceptT conn Read (getTraitMaybe "trait2223333")
  -- print trait
