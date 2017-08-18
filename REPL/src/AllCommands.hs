{-# LANGUAGE RecordWildCards #-}

module AllCommands(
                  UpdateInfo(..), 
                  CombinedPackage (..),
                  queryCombinedData,
                  allCommands, updateAllCommand) where

import Data.Default

import Common
import qualified HackageCommands as HC
import qualified StackageCommands as SC
import qualified HackageArchive as HA
import qualified StackageArchive as SA
import REPL

data UpdateInfo = UI {
  iuh :: HC.HackageUpdateInfo,
  sui :: SC.StackageUpdateInfo
} deriving (Eq, Show)

instance Default UpdateInfo where
  def = defaultUI

defaultUI :: UpdateInfo
defaultUI = UI { 
  iuh = def, 
  sui = def 
} 

newtype CombinedPackage =  CP (HA.HackagePackage, Maybe SA.StackagePackage) deriving (Eq)

instance Show CombinedPackage where
  show (CP (hp, sp)) = packageName ++ " " ++ show packageVersion ++ present
    where
      PackageId{..} = HA.package hp
      present = case sp of
        Just _  -> " present in stackage"
        Nothing -> " not in stackage"

queryCombinedData :: UpdateInfo -> PackageName -> IO (Maybe CombinedPackage)
queryCombinedData ui package = do
    hp <- HA.queryPersistentMap hUpdateDir package
    sp <- SA.queryPersistentMap sUpdateDir package
    return $ hp >>= \p -> Just $ CP (p, sp)
    where
        sUpdateDir = (SC.getLTSPersistDir.sui) ui
        hUpdateDir = (HC.getArchivePersistDir.iuh) ui


-- This method just shows the result of querying by queryCombinedData method
showQueryCombinedData :: UpdateInfo -> PackageName -> IO ()
showQueryCombinedData ui package = do
    putStrLn $ "Querying package " ++ package
    query <- queryCombinedData ui package
    print query

showQueryCombinedDataCommand :: REPLCommand UpdateInfo
showQueryCombinedDataCommand = RC {
  cTag = "all",
  cMatch = isPrefixCommand "query",
  cExec = \ui commandStr -> let package = parseValEnd commandStr in showQueryCombinedData ui package,
  cDescription = const "query package - queries package in stackage and hackage archives"
}

updateAllCommand :: REPLCommand UpdateInfo
updateAllCommand = RC {
  cTag = "all",
  cMatch = isTrimCommand "allupdate",
  cExec = \ui _ ->  cExec SC.updatePersistentMapFromLTSCommand (sui ui) "" >> 
                    cExec HC.updateAllHackageCommand (iuh ui) "",
  cDescription = const "allupdate - updates stackage and hackage"
}

transformH :: REPLCommand HC.HackageUpdateInfo -> REPLCommand UpdateInfo
transformH hCommand = RC {
  cTag = cTag hCommand, -- same as in the hackage command
  cMatch = cMatch hCommand,
  cExec = \ui commandStr -> cExec hCommand (iuh ui) commandStr,
  cDescription = cDescription hCommand . iuh  
}

transformS :: REPLCommand SC.StackageUpdateInfo -> REPLCommand UpdateInfo
transformS sCommand = RC {
  cTag = cTag sCommand, -- same as in the hackage command
  cMatch = cMatch sCommand,
  cExec = \ui commandStr -> cExec sCommand (sui ui) commandStr,
  cDescription = cDescription sCommand . sui  
}

allCommands :: [REPLCommand UpdateInfo]
allCommands = updateAllCommand : showQueryCombinedDataCommand :
  map transformH HC.hackageCommands ++ map transformS SC.stackageCommands
