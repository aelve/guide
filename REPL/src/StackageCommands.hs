module StackageCommands(
                        stackageCommands,
                        updatePersistentMapFromLTSCommand,
                        StackageUpdateInfo(..),
                        getSnapshotURL,
                        getLTSGithubURL,
                        getLTSFilesDir,
                        getLTSStackageURL,
                        getLTSFile,
                        getLTSPersistDir,
                        defaultSUI) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import System.FilePath((</>))
import Data.Default

import Common              
import StackageUpdate
import StackageArchive
import REPL

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

instance Default StackageUpdateInfo where
  def = defaultSUI

getLTSPersistDir :: StackageUpdateInfo -> FilePath
getLTSPersistDir sui = suiUpdateDir sui </> "persist"

defaultSUI :: StackageUpdateInfo
defaultSUI = SUI {
  suiUpdateDir = "stackagefiles", 
  suiStackageURL = "https://www.stackage.org/",
  suiLTSURL = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
}

stackageCommands :: [REPLCommand StackageUpdateInfo]
stackageCommands = [
    showSnapshotsCommand,
    showLTSContentsCommand,
    updateLTSFileCommand,
    updateAllLTSFilesCommand,
    showStackageMapContentsCommand,
    updatePersistentMapFromLTSCommand,
    showPersistentQueryCommand]

showSnapshots :: URL -> IO ()
showSnapshots url = do
    SSS snapshots <- fetchStackageSnapshots url
    putStrLn $ "Showing snapshots from " ++ url
    mapM_ (putStrLn.(\s -> "\tSnapshot: " ++ s).show) snapshots

showSnapshotsCommand :: REPLCommand StackageUpdateInfo
showSnapshotsCommand = RC {
    cTag = "stackage",
    cMatch = isTrimCommand "ltssnapshots",
    cExec = \sui _ -> showSnapshots (getSnapshotURL sui),
    cDescription = \sui -> "ltssnapshots - show the stackage snapshots from " ++ getSnapshotURL sui 
}

showLTSContents :: FilePath -> IO ()
showLTSContents ltsFile = do
    putStrLn $ "Showing the contents of " ++ ltsFile
    body <- BS.readFile ltsFile
    datum <- parseYamlFileThrow body
    print datum

showLTSContentsCommand :: REPLCommand StackageUpdateInfo
showLTSContentsCommand = RC {
    cTag = "stackage",
    cMatch = isPrefixCommand "ltsshowcont ",
    cExec = \sui commandStr -> let lts = parseValEnd commandStr in showLTSContents  (getLTSFile sui lts),
    cDescription = const "ltsshowcont lts - shows the contents of specified downloaded lts file" 
}

-- updates the lts file from github
updateLTSFile :: FilePath -> URL -> IO ()
updateLTSFile = fetchLTS 

updateLTSFileCommand :: REPLCommand StackageUpdateInfo
updateLTSFileCommand = RC {
    cTag = "stackage",
    cMatch = isPrefixCommand "ltsupdate ",
    cExec = \sui commandStr -> let lts = parseValEnd commandStr in 
        updateLTSFile (getLTSFile sui lts) (getLTSGithubURL sui lts) ,
    cDescription = const "ltsupdate lts - updates the specified lts file from github" 
}

-- updates all of the lts files from the snapshot file at stackage
updateAllLTSFiles :: FilePath -> URL -> URL -> IO ()
updateAllLTSFiles ltsDir ltsURL snapshotsURL = do
    snapshots <- fetchStackageSnapshots snapshotsURL
    fetchAllLTSFiles ltsDir ltsURL (filterNormal snapshots)

updateAllLTSFilesCommand :: REPLCommand StackageUpdateInfo
updateAllLTSFilesCommand = RC {
    cTag = "stackage",
    cMatch = isTrimCommand "ltsallupdate",
    cExec = \sui commandStr -> updateAllLTSFiles (getLTSFilesDir sui) (suiLTSURL sui) (getSnapshotURL sui),
    cDescription = const "ltsallupdate - updates all lts files from github using snapshot from stackage" 
}


showStackageMapContents :: FilePath -> URL -> URL -> Int -> IO()
showStackageMapContents ltsDir ltsURL snapshotsURL count = do
    putStrLn "Fetching snapshot lists"
    snapshots <- fetchStackageSnapshots snapshotsURL
    putStrLn "Downloading YAML files"
    fetchAllLTSFiles ltsDir ltsURL (filterNormal snapshots)
    putStrLn "Generating stackage map"
    map <- generateStackageMap ltsDir (filterNormal snapshots)
    putStrLn $ "Printing " ++ show count ++ " packages"
    mapM_ print $ take count $ M.toList map

showStackageMapContentsCommand :: REPLCommand StackageUpdateInfo
showStackageMapContentsCommand = RC {
    cTag = "stackage",
    cMatch = isTrimCommand "ltsshowmap",
    cExec = \sui commandStr -> 
        showStackageMapContents (getLTSFilesDir sui) (suiLTSURL sui) (getSnapshotURL sui) 20,
    cDescription = const $ "ltsshowmap - downloads lts files and shots the part of" ++ 
        " stackage package map from them" 
}

updatePersistentMapFromLTS :: FilePath -> FilePath -> URL -> URL -> IO()
updatePersistentMapFromLTS updateDir ltsDir ltsURL snapshotsURL  = do
    putStrLn "Fetching snapshot lists"
    snapshots <- fetchStackageSnapshots snapshotsURL
    putStrLn "Downloading YAML files"
    fetchAllLTSFiles ltsDir ltsURL (filterNormal snapshots)
    putStrLn "Generating stackage map"
    map <- generateStackageMap ltsDir (filterNormal snapshots)
    updatePersistentMap updateDir map 

updatePersistentMapFromLTSCommand :: REPLCommand StackageUpdateInfo
updatePersistentMapFromLTSCommand = RC {
    cTag = "stackage",
    cMatch = isTrimCommand "ltsupdatepersist",
    cExec = \sui commandStr -> 
        updatePersistentMapFromLTS 
            (suiUpdateDir sui) (getLTSFilesDir sui) (suiLTSURL sui) (getSnapshotURL sui),
    cDescription = const $ "ltsupdatepersist - gets all the lts snapshots from the stackage, " ++ 
        "updates the lts files according to them and then updates the persistent storage"
}

showPersistentQuery :: FilePath -> PackageName -> IO()
showPersistentQuery updateDir name = do
  putStrLn $ "Querying storage stackage map with " ++ name
  value <- queryPersistentMap updateDir name
  case value of 
    Just package -> do
      putStrLn "Found"
      print package
    Nothing -> putStrLn "Not found"

showPersistentQueryCommand :: REPLCommand StackageUpdateInfo
showPersistentQueryCommand = RC {
    cTag = "stackage",
    cMatch = isPrefixCommand "ltsquerypersist",
    cExec = \sui commandStr -> let lts = parseValEnd commandStr in 
        showPersistentQuery (suiUpdateDir sui) lts,
    cDescription = const "ltsquerypersist package - queries the persistent map of the stackage packages"
}
