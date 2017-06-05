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

import TarUtil
import qualified Data.Version as DV

data KeyValue = KeyValue !HackageMap
    deriving (Typeable)
type Key = String
type Value = HackagePackage


$(deriveSafeCopy 0 'base ''DV.Version)
$(deriveSafeCopy 0 'base ''HackagePackage)
$(deriveSafeCopy 0 'base ''KeyValue)
$(deriveSafeCopy 0 'base ''HackageUpdate)

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value = do 
  KeyValue hackageMap <- State.get
  State.put (KeyValue (M.insert key value hackageMap))

updateMap :: HackageMap -> Update KeyValue ()
updateMap newMap = State.put (KeyValue newMap)

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key = do
  KeyValue m <- ask
  return (M.lookup key m)

compareMap :: HackageMap -> Query KeyValue HackageUpdateMap
compareMap newMap = do
  KeyValue oldMap <- ask
  return (buildDifferenceMap oldMap newMap)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'compareMap, 'updateMap])

printAcidDiffMap :: HackageMap -> IO ()
printAcidDiffMap newMap = do
  acid <- openLocalState (KeyValue M.empty)
  do
    diffMap <- query acid (CompareMap newMap)
    putStrLn $ "Printing difference map with acid-state"
    mapM_ (print.snd) $ M.toList diffMap
  closeAcidState acid

updateAcidMap :: HackageMap -> IO ()
updateAcidMap newMap = do
  acid <- openLocalState (KeyValue M.empty)
  do
    putStrLn $ "Updating the acid map"
    update acid (UpdateMap newMap) 
  closeAcidState acid

queryAcidMap :: Key -> IO (Maybe Value)
queryAcidMap key = do
  acid <- openLocalState (KeyValue M.empty)
  val <- query acid (LookupKey key)
  closeAcidState acid
  return val
    
