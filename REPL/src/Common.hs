module Common(URL,
              PackageName,
              PackageData,
              SnapshotData(..),
              UpdateArchiveException(..),
              HackageUpdateInfo(..),
              getArchive,
              getArchiveClone,
              getTar,
              getTarClone,
              parseIntEnd,
              parseValEnd) where

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
