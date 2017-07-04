module Common(URL,
              PackageName,
              PackageData,
              PackageDatum(..),
              SnapshotData(..),
              UpdateArchiveException(..),
              parseIntEnd,
              parseValEnd,
              
              ShortSnapshotName,
              LongSnapshotName,
              shortName,
              longName,
              StackageSnapshot,
              StackageSnapshots(..), 
              getNormalSnapshots,
              filterNormal,
              StackageLTS) where

import qualified Control.Exception as X
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL

import Data.Version as DV
import Data.Int(Int64)

type URL = String 
type PackageName = String
type PackageData = (PackageName, DV.Version)

data SnapshotData = SnapshotData { 
  md5Hash :: String,
  lengthFile :: Int64
} deriving (Eq, Show)

-- The exception, that is raised, when there is problems with creating the
-- snapshot
newtype UpdateArchiveException = UAE String deriving (Show, Eq)
instance X.Exception UpdateArchiveException

-- the constructor short name is really awkward in russian


parseIntEnd :: (Num a, Read a) => String -> a
parseIntEnd val | not (null l) = read (DL.last l)
                | otherwise = 0
                where l = words val

parseValEnd :: String -> String
parseValEnd val | DL.length l > 1 = DL.last l
                | otherwise = ""
                where l = words val


-- Stackage stuff
type ShortSnapshotName = String
type LongSnapshotName = String
type StackageSnapshot = (ShortSnapshotName, LongSnapshotName)
newtype StackageSnapshots = SSS [StackageSnapshot] deriving (Eq, Show)

filterNormal :: StackageSnapshots -> StackageSnapshots
filterNormal (SSS ss) = SSS (filter (\(_, l) -> DL.isPrefixOf "lts" l) ss)

getSnapshots :: StackageSnapshots -> [StackageSnapshot] 
getSnapshots (SSS ss) = ss

getNormalSnapshots :: StackageSnapshots -> [StackageSnapshot]
getNormalSnapshots = getSnapshots.filterNormal

newtype PackageDatum = PD [PackageData] deriving (Eq, Show)

shortName :: StackageSnapshot -> String
shortName = fst

longName :: StackageSnapshot -> String
longName = snd

type StackageLTS = (LongSnapshotName, [PackageData])