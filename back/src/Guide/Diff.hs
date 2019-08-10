{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Diff- and merge-related things.
module Guide.Diff
(
  -- * Diffing
  Diff(..),
  DiffChunk(..),
  diff,

  -- * Merging
  merge,

  -- * Tokenizing
  tokenize,
)
where

-- shared imports
import Imports
-- Vector
import Data.Vector (Vector)

import Guide.Diff.Merge (merge)
import Guide.Diff.Tokenize (tokenize)
import Guide.Utils (makeClassWithLenses)

import qualified Data.Patch as PV
import qualified Data.Vector as V

-- | Result of a diff.
data Diff = Diff {
  diffContextAbove :: [Text],   -- ^ Context (unchanged parts)
                                --    above the differing part
  diffContextBelow :: [Text],   -- ^ Context below the differing part
  diffLeft         :: [DiffChunk],     -- ^ Will contain only 'Deleted' and 'Plain'
  diffRight        :: [DiffChunk]      -- ^ Will contain only 'Added' and 'Plain'
  }
  deriving (Show)

data DiffChunk
  = Deleted Text      -- ^ Something was deleted (from the left side)
  | Added   Text      -- ^ Something was added (to the right side)
  | Plain   Text      -- ^ This part should be rendered as not modified
  deriving (Eq, Show)

makeClassWithLenses ''Diff

diff
  :: Text    -- ^ Original text
  -> Text    -- ^ Edited text
  -> Diff
diff (tokenize -> orig) (tokenize -> edit) =
    trimDiff (diffL (PV.hunks diffBA (V.fromList edit')))
             (diffR (PV.hunks diffAB (V.fromList orig')))
      & _diffContextAbove %~ (prefix <>)
      & _diffContextBelow %~ (<> suffix)
  where
    -- we find common parts in advance because diffs are O(mn) and removing
    -- big unchanged parts in advance helps us
    (prefix, (orig', edit'), suffix) = commonParts orig edit
    -- then we compute orig→edit and edit→orig diffs
    diffAB = PV.diff (V.fromList orig') (V.fromList edit')
    diffBA = PV.inverse diffAB

-- | Create a diff for the right (edited) part. We only want to highlight
-- parts which were inserted or replaced.
diffR :: PV.Hunks Text -> [DiffChunk]
diffR = removeExtraAdded . concatMap hunkToChunk
  where
    hunkToChunk (v, PV.Inserted)  = [Added (tconcat v)]
    hunkToChunk (v, PV.Replaced)  = [Added (tconcat v)]
    hunkToChunk (v, PV.Unchanged) = map Plain (toList v)
    -- it's useful to report deleted things as well because then we can mark
    -- them with tiny rectangles like “insert here”
    hunkToChunk (_, PV.Deleted)   = [Added ""]
    -- however, we don't need them if there's already an addition marked there
    removeExtraAdded (Added "" : Added x : xs) =
      removeExtraAdded (Added x : xs)
    removeExtraAdded (Added x : Added "" : xs) =
      removeExtraAdded (Added x : xs)
    removeExtraAdded (x : xs) =
      x : removeExtraAdded xs
    removeExtraAdded [] = []

-- | Create a diff for the left (original) part. We only want to highlight
-- parts which were deleted or replaced.
--
-- This function should receive a diff that goes in reverse (i.e. from edited
-- text to original text)
diffL :: PV.Hunks Text -> [DiffChunk]
diffL = removeExtraDeleted . concatMap hunkToChunk
  where
    -- Since the diff is edit→orig, this code might make not much sense at
    -- first. When something was “inserted” to original text when going
    -- edit→orig, it actually means that it was deleted from the original
    -- text when going orig→edit, and thus we want to render it as deleted.
    hunkToChunk (v, PV.Inserted)  = [Deleted (tconcat v)]
    hunkToChunk (v, PV.Replaced)  = [Deleted (tconcat v)]
    hunkToChunk (v, PV.Unchanged) = map Plain (toList v)
    hunkToChunk (_, PV.Deleted)   = [Deleted ""]
    removeExtraDeleted (Deleted "" : Deleted x : xs) =
      removeExtraDeleted (Deleted x : xs)
    removeExtraDeleted (Deleted x : Deleted "" : xs) =
      removeExtraDeleted (Deleted x : xs)
    removeExtraDeleted (x : xs) =
      x : removeExtraDeleted xs
    removeExtraDeleted [] = []

-- | In a bunch of chunks, find only the part that was changed
trimDiff
  :: [DiffChunk]    -- ^ The diff after 'diffL'
  -> [DiffChunk]    -- ^ The diff after 'diffR'
  -> Diff
trimDiff a b =
  Diff {
    diffContextAbove = map getPlain prefix,
    diffContextBelow = map getPlain suffix,
    diffLeft  = a',
    diffRight = b'
    }
  where
    (prefix, (a', b'), suffix) = commonParts a b
    -- since chunks in 'a' contain Deleted and Plain, and chunks in 'b'
    -- contain Added and Plain, the only equal parts will be Plain
    getPlain (Plain x) = x
    getPlain x         = error ("trimDiff: impossible: " ++ show x)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

tconcat :: Vector Text -> Text
tconcat = mconcat . toList

-- | Find longest common prefix
commonPrefix :: Eq a => [a] -> [a] -> ([a], ([a], [a]))
commonPrefix = go []
  where
    go p [] bs = (reverse p, ([], bs))
    go p as [] = (reverse p, (as, []))
    go p (a:as) (b:bs)
      | a == b    = go (a:p) as bs
      | otherwise = (reverse p, (a:as, b:bs))

-- | Find longest common suffix
commonSuffix :: Eq a => [a] -> [a] -> (([a], [a]), [a])
commonSuffix a b = ((reverse neqA, reverse neqB), reverse eq)
  where
    (eq, (neqA, neqB)) = commonPrefix (reverse a) (reverse b)

-- | Find longest common prefix and suffix
commonParts :: Eq a => [a] -> [a] -> ([a], ([a], [a]), [a])
commonParts a b = (prefix, (a'', b''), suffix)
  where
    (prefix, (a', b'))   = commonPrefix a b
    ((a'', b''), suffix) = commonSuffix a' b'
