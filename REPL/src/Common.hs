module Common
(
  URL,
  PackageName,
  PackageId(..),
  SnapshotData(..),
  UpdateArchiveException(..),
  parseIntEnd,
  parseValEnd,

  SnapshotName,
  SnapshotId(..),
  filterLTS,
  StackageLTS,
)
where


import qualified Control.Exception as X
import Data.List

import Data.Version
import Data.Int


type URL = String 

type PackageName = String

data PackageId = PackageId {
  packageName    :: PackageName,
  packageVersion :: Version
  } deriving (Eq, Show)

data SnapshotData = SnapshotData { 
  md5Hash :: String,
  lengthFile :: Int64
  }
  deriving (Eq, Show)

-- The exception that is raised when there are problems with creating the
-- snapshot
newtype UpdateArchiveException = UAE String deriving (Show, Eq)
instance X.Exception UpdateArchiveException

parseIntEnd :: (Num a, Read a) => String -> a
parseIntEnd val | not (null l) = read (last l)
                | otherwise = 0
                where l = words val

parseValEnd :: String -> String
parseValEnd val | length l > 1 = last l
                | otherwise = ""
                where l = words val

type SnapshotName = String

data SnapshotId = SnapshotId {
  snapshotName  :: SnapshotName,  -- ^ E.g. “lts-8.23” or “nightly-2017-07-21”
  snapshotGroup :: String         -- ^ E.g. “lts-8” or “nightly”
  }
  deriving (Eq, Show)

filterLTS :: [SnapshotId] -> [SnapshotId]
filterLTS = filter (isPrefixOf "lts-" . snapshotName)

data StackageLTS = StackageLTS {
  snapshot :: SnapshotId,
  packages :: [PackageId]
  } deriving (Eq, Show)
