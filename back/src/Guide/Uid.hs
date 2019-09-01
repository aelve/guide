{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A type for unique identifiers.
module Guide.Uid
(
  Uid(..),
  randomShortUid,
  randomLongUid,
)
where

import Imports
import Data.Aeson
import Data.SafeCopy
import Web.HttpApiData
import System.Random (randomRIO)

----------------------------------------------------------------------------
-- Type
----------------------------------------------------------------------------

-- | Unique id, used for many things – categories, items, and anchor ids.
newtype Uid a = Uid {uidToText :: Text}
  deriving stock (Generic, Eq, Ord, Data)
  deriving newtype
    (Read, Show, IsString, Buildable, ToHttpApiData, FromHttpApiData,
     Hashable, ToJSON, FromJSON, NFData)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

-- This instance is written manually because otherwise it produces a warning:
--     • Redundant constraint: SafeCopy a
--     • In the instance declaration for ‘SafeCopy (Uid a)’
instance SafeCopy (Uid a) where
  putCopy = contain . safePut . uidToText
  getCopy = contain (Uid <$> safeGet)
  version = 2
  kind = base

----------------------------------------------------------------------------
-- Generating random uids
----------------------------------------------------------------------------

-- | Generate a random text of given length from characters @a-z@ and digits.
randomText :: MonadIO m => Int -> m Text
randomText n = liftIO $ do
  -- We don't want the 1st char to be a digit. Just in case (I don't really
  -- have a good reason). Maybe to prevent Javascript from doing automatic
  -- conversions or something (though it should never happen).
  x <- randomRIO ('a', 'z')
  let randomChar = do
        i <- randomRIO (0, 35)
        return $ if i < 10 then toEnum (fromEnum '0' + i)
                           else toEnum (fromEnum 'a' + i - 10)
  xs <- replicateM (n-1) randomChar
  return (toText (x:xs))

-- For probability tables, see
-- https://en.wikipedia.org/wiki/Birthday_problem#Probability_table

-- | Generate a random UID of length 12.
--
-- Probability of collision for
--
--   * a million UIDs: approximately 1e-6
--   * a billion UIDs: approximately 0.25
randomLongUid :: MonadIO m => m (Uid a)
randomLongUid = Uid <$> randomText 12

-- | Generate a random UID of length 8.
--
-- These UIDs are only used for items and categories (because their uids can
-- occur in links and so they should look a bit nicer).
--
-- Probability of collision for
--
--   * a hundred thousand UIDs: approximately 0.5%
--   * a million UIDs: approximately 40%
randomShortUid :: MonadIO m => m (Uid a)
randomShortUid = Uid <$> randomText 8
