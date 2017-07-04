{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module StackageArchive(    
    generateStackageMap,
    updatePersistentMap,
    queryPersistentMap,
    StackagePackage(..),
    StackageMap
) where

import qualified Data.Map.Strict as M
import qualified Data.Version as DV
import qualified Data.ByteString as BS
import System.FilePath((</>))

import Data.Typeable
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import qualified Control.Monad.State  as State
import Control.Monad.Reader


import Common
import StackageUpdate

-- This is a mapping of version of the package, that is present in the lts
-- snapshot with the specified name. So 
newtype StackageVersionLTS = SVL (M.Map LongSnapshotName DV.Version) deriving (Eq)

instance Show StackageVersionLTS where
  show (SVL map) = "LTS versions\n" ++ concatMap (\(n, v) -> n ++ " " ++ show v ++ "\n") (M.toList map)

makeSVL :: LongSnapshotName -> DV.Version -> StackageVersionLTS
makeSVL ss v = SVL $ M.singleton ss v

data StackagePackage = SP {
  name :: PackageName,
  ltsVersions :: StackageVersionLTS
} deriving (Eq)

instance Show StackagePackage where
    show (SP name versions) = "SP " ++ name ++ "\n" ++ show versions
        
addSVL :: StackagePackage -> LongSnapshotName -> DV.Version -> StackagePackage
addSVL (SP n (SVL m)) name version = SP n $ SVL $ M.insert name version m

type StackageMap = M.Map PackageName StackagePackage

updateStackageMap :: StackageMap -> LongSnapshotName -> PackageDatum -> StackageMap
updateStackageMap map snapshotName (PD packages) = 
    foldr (\p m -> updateStackageMapPackage m snapshotName p) map packages

updateStackageMapPackage :: StackageMap -> LongSnapshotName -> PackageData -> StackageMap
updateStackageMapPackage map snapshot (package, version) = case M.lookup package map of
    -- The package is present in map, extend it's knoledge by adding new snapshot info
    -- into it's map
    Just sp -> M.update (\sp -> Just $ addSVL sp snapshot version) package map
    -- well, no package is present. Create one from scratch
    Nothing -> M.insert package (SP package (makeSVL snapshot version)) map

generateStackageMap :: FilePath -> StackageSnapshots -> IO StackageMap
-- make the empty map here
generateStackageMap _ (SSS []) = return M.empty 
generateStackageMap filePath (SSS (s: xs)) = do
    -- get the yaml file
    body <- BS.readFile (filePath </> longName s ++ ".yaml")
    newMap <- generateStackageMap filePath $ SSS xs
    -- build the map from this yaml file
    pkgDatum <- parseYamlFileThrow body
    return $ updateStackageMap newMap (longName s) pkgDatum

-- this is needed for acid serialization
newtype KeyValue = KeyValue StackageMap deriving (Typeable)

$(deriveSafeCopy 0 'base ''StackageVersionLTS)
$(deriveSafeCopy 0 'base ''StackagePackage)
$(deriveSafeCopy 0 'base ''DV.Version)
$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: PackageName -> StackagePackage -> Update KeyValue ()
insertKey key value = do 
  KeyValue stackageMap <- State.get
  State.put (KeyValue (M.insert key value stackageMap))

updateMap :: StackageMap -> Update KeyValue ()
updateMap newMap = State.put (KeyValue newMap)

lookupKey :: PackageName -> Query KeyValue (Maybe StackagePackage)
lookupKey key = do
  KeyValue m <- ask
  return (M.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'updateMap])

updatePersistentMap :: FilePath -> StackageMap -> IO ()
updatePersistentMap path newMap = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  do
    putStrLn "Updating the persistent map"
    update acid (UpdateMap newMap) 
  closeAcidState acid

queryPersistentMap :: FilePath -> PackageName -> IO (Maybe StackagePackage)
queryPersistentMap path name = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  val <- query acid (LookupKey name)
  closeAcidState acid
  return val
