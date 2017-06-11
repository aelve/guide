{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module HackageArchive (
                buildDifferenceMap,
                buildHackageMap,
                buildPreHackageMap,
                
                updatePersistentMap,
                printPersistentDiffMap,
                queryPersistentMap,

                HackagePackage (..),
                HackageName,
                HackageMap,
                HackageUpdateMap,
                HackageUpdate,
                parsePath) where

import qualified Codec.Archive.Tar as Tar
import qualified Data.List.Split as SPLT
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Version as DV
import qualified Distribution.PackageDescription.Parse as CP
import qualified Distribution.Package as DP

import qualified Text.ParserCombinators.ReadP as RP
import qualified Distribution.PackageDescription as DPD
import qualified Distribution.PackageDescription.Parse as DPDP
import qualified Data.Map.Strict as M
import qualified Control.Exception as X

import Data.Typeable
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Control.Monad.Reader

import Data.Maybe
import Debug.Trace
import Control.Monad(guard)

import qualified Data.ByteString.Lazy.UTF8 as UTFC
import qualified Control.Monad.State  as State

import System.FilePath.Posix(hasTrailingPathSeparator)
import Common

type HackageName = String

-- The record for each of the package from hackage
-- TODO - add another information about the packages
data HackagePackage = HP {
  name :: HackageName,
  pVersion :: DV.Version,
  author :: String
} deriving (Eq, Show)

-- The status of the package between two updates
data HackageUpdate = Added | Removed | Updated deriving (Eq, Show)

-- The map of all the hackage packages with name as the key and HackagePackage
-- as the value
type HackageMap = M.Map HackageName HackagePackage
type PreHackageMap = M.Map HackageName PackageVersion

-- The map, that shows, which packages have change since the last update
type HackageUpdateMap = M.Map HackageName (HackageUpdate, HackagePackage)

-- Parses the file path of the cabal file to get version and package name
parseCabalFilePath :: RP.ReadP PackageData
parseCabalFilePath = do
  package <- RP.munch1 phi
  RP.char '/'
  version <- DV.parseVersion
  RP.char '/'
  name <- RP.munch1 phi
  guard (name == package)
  suff <- RP.string ".cabal"
  RP.eof
  pure (package, Specified version)
  where phi l = DC.isLetter l || l == '-'

updateMapCompare :: (Ord a) => String -> a -> M.Map String a -> M.Map String a
updateMapCompare key value map = case M.lookup key map of
  Just oldValue -> if value > oldValue  then updatedMap
                                        else map
  Nothing -> updatedMap
  where updatedMap = M.insert key value map


buildDifferenceMap :: HackageMap -> HackageMap -> HackageUpdateMap
buildDifferenceMap oldMap newMap = foldr M.union M.empty [deletedMap, addedMap, updatedMap]
  where
    deletedMap = M.map ((,) Removed) $ M.difference oldMap newMap
    addedMap = M.map ((,) Added) $ M.difference newMap oldMap
    updatedMap' = M.intersection newMap oldMap
    updatedMap = M.map ((,) Updated) $ M.differenceWith diff updatedMap' oldMap
    diff newpack oldpack = if newpack /= oldpack then Just newpack else Nothing

createPackage :: DPD.PackageDescription -> HackagePackage
createPackage pd = HP { name = nm, pVersion = ver, author = auth }
  where
    pkg = DPD.package pd
    nm = DP.unPackageName (DP.pkgName pkg)
    ver = DP.pkgVersion pkg
    auth = DPD.author pd

parsePath :: FilePath -> Maybe PackageData
parsePath path = case RP.readP_to_S parseCabalFilePath path of 
    [(pd, _)] -> Just pd
    _ -> Nothing

parsePackageDescription :: Tar.EntryContent -> Maybe DPD.PackageDescription
parsePackageDescription (Tar.NormalFile content _) = 
  case DPDP.parsePackageDescription (UTFC.toString content) of 
    DPDP.ParseOk _ pd -> Just (DPD.packageDescription pd)
    DPDP.ParseFailed _ -> Nothing
parsePackageDescription _ = Nothing

parsePackage :: Tar.Entry -> Maybe HackagePackage
parsePackage entry = do
  (path, version) <- parsePath $ Tar.entryPath entry
  pd <- parsePackageDescription $ Tar.entryContent entry
  return $ createPackage pd

updatePreMap :: PackageData -> PreHackageMap -> PreHackageMap
updatePreMap (name, version) = updateMapCompare name version

buildPreHackageMap :: Tar.Entries Tar.FormatError -> PreHackageMap
buildPreHackageMap (Tar.Next entry entries) = 
  case parsePath $ Tar.entryPath entry of
    Just hp -> updatePreMap hp map
    Nothing -> map
  where map = buildPreHackageMap entries
buildPreHackageMap Tar.Done = M.empty
buildPrehackageMap (Tar.Fail e) = X.throw e


buildHackageMap :: Tar.Entries Tar.FormatError -> PreHackageMap -> HackageMap
buildHackageMap (Tar.Next entry entries) premap = 
  case update $ Tar.entryPath entry of
    Just hp -> M.insert (name hp) hp map
    Nothing -> map
  where map = buildHackageMap entries premap
        update path = do
          (name, version) <- parsePath path
          preversion <- M.lookup name premap
          if preversion == version  then parsePackage entry
                                      else Nothing
buildHackageMap Tar.Done _ = M.empty
buildHackageMap (Tar.Fail e) _ = X.throw e


-- The stuff needed for acid serialization
newtype KeyValue = KeyValue HackageMap deriving (Typeable)

$(deriveSafeCopy 0 'base ''DV.Version)
$(deriveSafeCopy 0 'base ''HackagePackage)
$(deriveSafeCopy 0 'base ''KeyValue)
$(deriveSafeCopy 0 'base ''HackageUpdate)

insertKey :: HackageName -> HackagePackage -> Update KeyValue ()
insertKey key value = do 
  KeyValue hackageMap <- State.get
  State.put (KeyValue (M.insert key value hackageMap))

updateMap :: HackageMap -> Update KeyValue ()
updateMap newMap = State.put (KeyValue newMap)

lookupKey :: HackageName -> Query KeyValue (Maybe HackagePackage)
lookupKey key = do
  KeyValue m <- ask
  return (M.lookup key m)

compareMap :: HackageMap -> Query KeyValue HackageUpdateMap
compareMap newMap = do
  KeyValue oldMap <- ask
  return (buildDifferenceMap oldMap newMap)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'compareMap, 'updateMap])


updatePersistentMap :: FilePath -> HackageMap -> IO ()
updatePersistentMap path newMap = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  do
    putStrLn "Updating the persistent map"
    update acid (UpdateMap newMap) 
  closeAcidState acid

printPersistentDiffMap :: FilePath -> HackageMap -> IO ()
printPersistentDiffMap path newMap = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  do
    diffMap <- query acid (CompareMap newMap)
    putStrLn "Printing difference map with persistent map"
    mapM_ (print.snd) $ M.toList diffMap
  closeAcidState acid

queryPersistentMap :: FilePath -> HackageName -> IO (Maybe HackagePackage)
queryPersistentMap path name = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  val <- query acid (LookupKey name)
  closeAcidState acid
  return val
  