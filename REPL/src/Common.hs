module Common(
              URL,
              PackageName,
              PackageData,
              SnapshotData(..),
              UpdateArchiveException(..)
              ) where
import Data.Version as DV
import Data.Int(Int64)
import qualified Control.Exception as X
import qualified Data.ByteString.Lazy as BL

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



