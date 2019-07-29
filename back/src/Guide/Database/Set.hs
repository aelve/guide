{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Update queries.
module Guide.Database.Set
       (
       -- * Trait
       -- * Item
       -- * Category
       setCategoryTitle
       , setCategoryGroup
       , setCategoryNotes
       , setCategoryStatus
       , changeCategoryEnabledSections

       ) where

import Imports

import Contravariant.Extras.Contrazip (contrazip2)
import Hasql.Statement (Statement (..))
import Hasql.Transaction (Transaction)
import Text.RawString.QQ (r)
import Hasql.Transaction.Sessions (Mode(..))

import qualified Data.Set as Set
import qualified Hasql.Decoders as HD
import qualified Hasql.Transaction as HT

import Guide.Database.Convert
import Guide.Database.Connection (connect, runTransactionExceptT)
import Guide.Database.Get (getCategory)
import Guide.Database.Types
import Guide.Types.Core (Category (..), CategoryStatus (..), Item (..), ItemSection(..), Trait (..),
                         TraitType (..))
import Guide.Utils (Uid (..))


-- | Test add functions
testSet :: IO ()
testSet = do
  conn <- connect
  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryTitle cat
  -- runTransactionExceptT conn Write (setCategoryTitle "category1111" "addedCatNew")
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryTitle cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryGroup_ cat
  -- runTransactionExceptT conn Write (setCategoryGroup "category1111" "groupNew2")
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryGroup_ cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryNotes cat
  -- runTransactionExceptT conn Write (setCategoryNotes "category1111" "new note")
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryNotes cat'

  -- cat <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryStatus cat
  -- runTransactionExceptT conn Write (setCategoryStatus "category1111" CategoryStub)
  -- cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  -- print $ _categoryStatus cat'

  cat <- runTransactionExceptT conn Read (getCategory "category1111")
  print $ _categoryEnabledSections cat
  runTransactionExceptT conn Write
    (changeCategoryEnabledSections "category1111"
      (Set.fromList [ItemProsConsSection, ItemNotesSection])
      (Set.fromList [ItemEcosystemSection]))
  cat' <- runTransactionExceptT conn Read (getCategory "category1111")
  print $ _categoryEnabledSections cat'

-- | Set new category title.
setCategoryTitle :: Uid Category -> Text -> ExceptT DatabaseError Transaction ()
setCategoryTitle catId title = do
  let sql = [r|
        UPDATE categories
        SET title = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (catId, title) (Statement sql encoder decoder False)

-- | Set new category group.
setCategoryGroup :: Uid Category -> Text -> ExceptT DatabaseError Transaction ()
setCategoryGroup catId group_ = do
  let sql = [r|
        UPDATE categories
        SET group_ = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (catId, group_) (Statement sql encoder decoder False)

-- | Set new category notes.
setCategoryNotes :: Uid Category -> Text -> ExceptT DatabaseError Transaction ()
setCategoryNotes catId notes = do
  let sql = [r|
        UPDATE categories
        SET notes = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam textParam
      decoder = HD.noResult
  lift $ HT.statement (catId, notes) (Statement sql encoder decoder False)

-- | Set new category notes.
setCategoryStatus :: Uid Category -> CategoryStatus -> ExceptT DatabaseError Transaction ()
setCategoryStatus catId status = do
  let sql = [r|
        UPDATE categories
        SET status_ = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam categoryStatusParam
      decoder = HD.noResult
  lift $ HT.statement (catId, status) (Statement sql encoder decoder False)

changeCategoryEnabledSections
  :: Uid Category
  -> Set ItemSection     -- ^ Sections to enable
  -> Set ItemSection     -- ^ Sections to disable
  -> ExceptT DatabaseError Transaction ()
changeCategoryEnabledSections catId toEnable toDisable = do
  oldSections <- _categoryEnabledSections <$> getCategory catId
  let newSections = (oldSections <> toEnable) Set.\\ toDisable
  let sql = [r|
        UPDATE categories
        SET enabled_sections = $2
        WHERE uid = $1
        |]
      encoder = contrazip2 uidParam itemSectionSetParam
      decoder = HD.noResult
  lift $ HT.statement (catId, newSections) (Statement sql encoder decoder False)
