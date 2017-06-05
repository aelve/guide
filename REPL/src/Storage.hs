{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

-- This is modified example from AcidState
module Storage (
                insertKey,
                updateMap,
                lookupKey,
                compareMap) where

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

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])


{-
main :: IO ()
main = do acid <- openLocalState (KeyValue Map.empty)
          updated <- performArchiveCutUpdate snapU archU arch cutValue

          -- load the map from acid
          -- update the map from acid
          closeAcidState acid
       where
        pbi = defaultPBI
        archU = (archiveURL pbi)
        snapU = (snapshotURL pbi)
        arch = (archive pbi)
        cutValue = 100000
-}
          {-
          case args of
            [key]
              -> do mbKey <- query acid (LookupKey key)
                    case mbKey of
                      Nothing    -> putStrLn $ key ++ " has no associated value."
                      Just value -> putStrLn $ key ++ " = " ++ value
            [key,val]
              -> do update acid (InsertKey key val)
                    putStrLn "Done."
            _ -> do putStrLn "Usage:"
                    putStrLn "  key               Lookup the value of 'key'."
                    putStrLn "  key value         Set the value of 'key' to 'value'."
          closeAcidState acid
          -}

