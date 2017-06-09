{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

-- This is modified example from AcidState
module Storage (
                printAcidDiffMap,
                updateAcidMap,
                queryAcidMap) where

import Data.Typeable
import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Control.Monad.State  as State

import HackageArchive
import qualified Data.Version as DV

data KeyValue = KeyValue !HackageMap
    deriving (Typeable)

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

printAcidDiffMap :: FilePath -> HackageMap -> IO ()
printAcidDiffMap path newMap = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  do
    diffMap <- query acid (CompareMap newMap)
    putStrLn $ "Printing difference map with acid-state"
    mapM_ (print.snd) $ M.toList diffMap
  closeAcidState acid

updateAcidMap :: FilePath -> HackageMap -> IO ()
updateAcidMap path newMap = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  do
    putStrLn $ "Updating the acid map"
    update acid (UpdateMap newMap) 
  closeAcidState acid

queryAcidMap :: FilePath -> HackageName -> IO (Maybe HackagePackage)
queryAcidMap path name = do
  acid <- openLocalStateFrom path (KeyValue M.empty)
  val <- query acid (LookupKey name)
  closeAcidState acid
  return val
    
