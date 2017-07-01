module Common(URL,
              PackageName,
              PackageData,
              PackageDatum(..),
              SnapshotData(..),
              UpdateArchiveException(..),
              UpdateInfo(..),
              HackageUpdateInfo(..),
              getArchive,
              getArchiveClone,
              getTar,
              getArchivePersistDir,
              getTarClone,
              parseIntEnd,
              parseValEnd,
              
              ShortSnapshotName,
              LongSnapshotName,
              shortName,
              longName,
              getSnapshotURL,
              getLTSGithubURL,
              getLTSFilesDir,
              getLTSStackageURL,
              getLTSFile,
              getLTSPersistDir,
              StackageSnapshot,
              StackageSnapshots(..), 
              getNormalSnapshots,
              filterNormal,
              StackageLTS, 
              StackageUpdateInfo(..)) where

import qualified Control.Exception as X
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL

import Data.Version as DV
import Data.Int(Int64)

import Data.Default
import System.FilePath((</>))

type URL = String 
type PackageName = String
type PackageData = (PackageName, DV.Version)

data UpdateInfo = UI {
  iuh :: HackageUpdateInfo,
  sui :: StackageUpdateInfo
} deriving (Eq, Show)

instance Default UpdateInfo where
  def = defaultUI

defaultUI :: UpdateInfo
defaultUI = UI { 
  iuh = defaultIUH, 
  sui = defaultSUI 
} 

data SnapshotData = SnapshotData { 
  md5Hash :: String,
  lengthFile :: Int64
} deriving (Eq, Show)

-- The exception, that is raised, when there is problems with creating the
-- snapshot
newtype UpdateArchiveException = UAE String deriving (Show, Eq)
instance X.Exception UpdateArchiveException

-- the constructor short name is really awkward in russian
data HackageUpdateInfo = IUH { 
  iuhUpdateDir :: FilePath,
  iuhSnapshotURL :: URL,
  iuhArchiveURL :: URL
} deriving (Eq, Show)


instance Default HackageUpdateInfo where
  def = defaultIUH

defaultIUH :: HackageUpdateInfo
defaultIUH = IUH {
  iuhUpdateDir = "hackagefiles",
  iuhSnapshotURL = "https://hackage.haskell.org/snapshot.json",
  iuhArchiveURL = "https://hackage.haskell.org/01-index.tar.gz"
}

archive :: FilePath
archive = "01-index.tar.gz"

archiveClone :: FilePath
archiveClone = "01-index.tar.gz.orig"

tar :: FilePath
tar = "01-index.tar"

tarClone :: FilePath
tarClone = "01-index.orig.tar" 

getArchivePersistDir :: HackageUpdateInfo -> FilePath
getArchivePersistDir iuh = iuhUpdateDir iuh </> "persist"


getArchive :: HackageUpdateInfo -> FilePath
getArchive iuh = iuhUpdateDir iuh </> archive

getArchiveClone :: HackageUpdateInfo -> FilePath
getArchiveClone iuh = iuhUpdateDir iuh </> archiveClone

getTar :: HackageUpdateInfo -> FilePath
getTar iuh = iuhUpdateDir iuh </> tar

getTarClone :: HackageUpdateInfo -> FilePath
getTarClone iuh = iuhUpdateDir iuh </> tarClone


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

getLTSStackageURL :: StackageUpdateInfo -> LongSnapshotName -> URL
getLTSStackageURL sui name = suiStackageURL sui </> name </> "cabal.config"

getSnapshotURL :: StackageUpdateInfo -> URL
getSnapshotURL sui = suiStackageURL sui </> "download/lts-snapshots.json"

getLTSGithubURL :: StackageUpdateInfo -> LongSnapshotName -> URL
getLTSGithubURL sui name = suiLTSURL sui </> (name ++ ".yaml") 

getLTSFilesDir :: StackageUpdateInfo -> FilePath
getLTSFilesDir sui = suiUpdateDir sui </> "ltsfiles"

getLTSFile :: StackageUpdateInfo -> String -> FilePath
getLTSFile sui lts = getLTSFilesDir sui </> (lts ++ ".yaml")

data StackageUpdateInfo = SUI { 
  suiUpdateDir :: FilePath, 
  suiStackageURL :: URL,
  suiLTSURL :: URL
} deriving (Eq, Show)

getLTSPersistDir :: StackageUpdateInfo -> FilePath
getLTSPersistDir sui = suiUpdateDir sui </> "persist"

defaultSUI :: StackageUpdateInfo
defaultSUI = SUI {
  suiUpdateDir = "stackagefiles", 
  suiStackageURL = "https://www.stackage.org/",
  suiLTSURL = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
}