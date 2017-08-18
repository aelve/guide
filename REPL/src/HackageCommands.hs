module HackageCommands(
                        hackageCommands,
                        updateAllHackageCommand,
                        HackageUpdateInfo(..),
                        getArchive,
                        getArchiveClone,
                        getTar,
                        getArchivePersistDir,
                        getTarClone,
                        defaultIUH) where

import qualified Data.Map.Strict as M
import Data.Int(Int64)
import System.Directory(copyFile)
import Control.Monad(void)
import Data.Default
import System.FilePath((</>))

import FileUtils
import Common
import HackageArchive
import HackageUpdate
import REPL

data HackageUpdateInfo = IUH { 
  iuhUpdateDir :: FilePath,
  iuhSnapshotURL :: URL,
  iuhArchiveURL :: URL
} deriving (Eq, Show)

instance Default HackageUpdateInfo where
  def = defaultIUH

hackageCommands :: [REPLCommand HackageUpdateInfo]
hackageCommands = [
  showTarElementsCommand, 
  showTarPreElementsCommand, 
  showArchiveSnapshotCommand, 
  showArchiveSnapshotCloneCommand,
  showUpdateArchiveCommand, 
  showUpdateArchiveCloneCommand,
  unzipArchiveCommand,
  unzipArchiveCloneCommand,
  copyArchiveCommand,
  showDiffMapCommand,
  cutArchiveCommand,
  cutArchiveCloneCommand,
  removeArchiveFilesCommand,
  removeArchiveFilesCloneCommand,
  archiveCompareCommand,
  updateArchiveCommand,
  updatePersistentFromArchiveCommand,
  showPersistentQueryCommand,
  showPersistentTarCompareCommand,
  updateAllHackageCommand
  ]

defaultIUH :: HackageUpdateInfo
defaultIUH = IUH {
  iuhUpdateDir = "hackagefiles",
  iuhSnapshotURL = "https://hackage.haskell.org/snapshot.json",
  iuhArchiveURL = "https://hackage.haskell.org/01-index.tar.gz"
}

getArchivePersistDir :: HackageUpdateInfo -> FilePath
getArchivePersistDir iuh = iuhUpdateDir iuh </> "persist"

getArchive :: HackageUpdateInfo -> FilePath
getArchive iuh = iuhUpdateDir iuh </> "01-index.tar.gz"

getArchiveClone :: HackageUpdateInfo -> FilePath
getArchiveClone iuh = iuhUpdateDir iuh </> "01-index.tar.gz.orig"

getTar :: HackageUpdateInfo -> FilePath
getTar iuh = iuhUpdateDir iuh </> "01-index.tar"

getTarClone :: HackageUpdateInfo -> FilePath
getTarClone iuh = iuhUpdateDir iuh </> "01-index.orig.tar" 

-- shows the first count elements, parsed from the tar archive
showTarElements :: FilePath -> Int -> IO ()
showTarElements path count = do
  putStrLn $ "Displaying " ++ show count ++ " entries for " ++ path
  tar <- loadTar path
  mapM_ (print.snd) $ take count $ M.toList $ buildHackageMap tar (buildPreHackageMap tar)

showTarElementsCommand :: REPLCommand HackageUpdateInfo
showTarElementsCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "tarshow",
  cExec = \iuh _ -> showTarElements (getTar iuh) 50,
  cDescription = \iuh -> "tarshow - loads the map of entries from " ++ getTar iuh ++ " and displays it" 
}

-- shows the first count pre elements (only path is parsed) form the tar archive
showTarPreElements :: FilePath -> Int -> IO ()
showTarPreElements path count = do
  putStrLn $ "Pre displaying " ++ show count ++ " entries for " ++ path
  tar <- loadTar path
  mapM_ print $ take count $ M.toList $ buildPreHackageMap tar

showTarPreElementsCommand :: REPLCommand HackageUpdateInfo
showTarPreElementsCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "tarshowpre",
  cExec = \iuh _ -> showTarPreElements (getTar iuh) 50,
  cDescription = \iuh -> "tarshowpre - loads the  premap of entries from " ++ 
    getTar iuh ++ " and displays it" 
}

-- Displays the snapshot of the file
showFileSnapshot :: FilePath -> IO()
showFileSnapshot file = do
  filesnapshot <- calcFileData file
  putStrLn $ "File result for " ++ file
  putStrLn $ "\tFile snapshot: " ++ show filesnapshot

showArchiveSnapshotCommand :: REPLCommand HackageUpdateInfo
showArchiveSnapshotCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "archivesnapshot",
  cExec = \iuh _ -> showFileSnapshot (getArchive iuh),
  cDescription = \iuh -> "archivesnapshot - displays the current " ++ getArchive iuh ++ " length and md5 hash" 
}

showArchiveSnapshotCloneCommand :: REPLCommand HackageUpdateInfo
showArchiveSnapshotCloneCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-archivesnapshot",
  cExec = \iuh _ -> showFileSnapshot (getArchiveClone iuh),
  cDescription = \iuh -> "system-archivesnapshot - displays the current " ++ 
    getArchiveClone iuh ++ " length and md5 hash" 
}

-- Shows the update data for the archive on disk
showUpdateData :: FilePath -> URL -> IO()
showUpdateData file json = do
  (range, snapshot, filesnapshot) <- calcUpdateResultIO file json
  putStrLn $ "Update result for file " ++ file
  putStrLn $ "\tHackage snapshot: " ++ show snapshot
  putStrLn $ "\tFile snapshot: " ++ show filesnapshot
  putStrLn $ "\tRange to update: " ++ show range

-- checks the current hackage gzip archive and understands what to download
showUpdateArchiveCommand :: REPLCommand HackageUpdateInfo
showUpdateArchiveCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "check",
  cExec = \iuh _ -> showUpdateData (getArchive iuh) (iuhSnapshotURL iuh),
  cDescription = \iuh -> "check - downloads the json length and md5 hash from " ++ 
    iuhSnapshotURL iuh ++ ", and compares it with local " ++ getArchive iuh
}

showUpdateArchiveCloneCommand :: REPLCommand HackageUpdateInfo
showUpdateArchiveCloneCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-checkclone",
  cExec = \iuh _ -> showUpdateData (getArchive iuh) (iuhSnapshotURL iuh),
  cDescription = \iuh -> "system-checkclone - downloads the json length and md5 hash from " ++ 
    iuhSnapshotURL iuh ++ ", and compares it with local " ++ getArchiveClone iuh
}

-- shows the substring of specified length from file from offset 
showFileSubstring :: FilePath -> Int64 -> Int64 -> IO ()
showFileSubstring file from length = do
  putStrLn $ "Showing " ++ file ++ " substr"
  putStr "\t"
  substr <- getFileSubstring file from length
  print substr

-- Copies the archive from first filename to the second
copyArchive :: FilePath -> FilePath -> IO ()
copyArchive archive1 archive2 = do
  copyFile archive1 archive2
  putStrLn $ "Copied the " ++ archive1 ++ " to " ++ archive2

copyArchiveCommand :: REPLCommand HackageUpdateInfo
copyArchiveCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-copyorig",
  cExec = \iuh _ -> copyArchive (getArchive iuh) (getArchiveClone iuh),
  cDescription = \iuh -> "system-copyorig - copy the " ++ getArchive iuh ++ 
    " to " ++ getArchiveClone iuh
}

-- Shows the difference between two tar archives, by building the pre maps of 
-- each of them, and then comparing
showDiffMap :: FilePath -> FilePath -> IO ()
showDiffMap newTarFile oldTarFile = do
  putStrLn $ "Displaying difference between " ++ newTarFile ++ " and " ++ oldTarFile
  newTar <- loadTar newTarFile
  oldTar <- loadTar oldTarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  let oldMap = buildHackageMap oldTar (buildPreHackageMap oldTar)
  let diffMap = buildDifferenceMap oldMap newMap
  mapM_ (print.snd) $ M.toList diffMap

showDiffMapCommand :: REPLCommand HackageUpdateInfo
showDiffMapCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-tarcmp",
  cExec = \iuh _ -> showDiffMap (getTar iuh) (getTarClone iuh),
  cDescription = \iuh -> "system-tarcmp - compares the entries of " ++
    getTar iuh ++ " and " ++ getTarClone iuh
}
-- this method cuts the data from the end of the archive,
-- because hackage 01-index.tar.gz is not strictly incremental
cutFile :: FilePath -> Int64 -> IO()
cutFile path size = do
  truncateIfExists path size
  putStrLn $ "Cut " ++ show size ++ " bytes from " ++ path

cutArchiveCommand :: REPLCommand HackageUpdateInfo
cutArchiveCommand = RC {
  cTag = "hackage",
  cMatch = isPrefixCommand "cut ",
  cExec = \iuh command -> cutFile (getArchive iuh) (parseIntEnd command),
  cDescription = \iuh -> "cut size - cuts the size bytes from the end of the " ++ 
    getArchive iuh ++ ", for update command"
}

cutArchiveCloneCommand :: REPLCommand HackageUpdateInfo
cutArchiveCloneCommand = RC {
  cTag = "system",
  cMatch = isPrefixCommand "system-cutclone ",
  cExec = \iuh command -> cutFile (getArchiveClone iuh) (parseIntEnd command),
  cDescription = \iuh -> "system-cutclone size - cuts the size bytes from the end of the " ++ 
    getArchiveClone iuh ++ ", for update command"
}

-- Unzips the gz archive to tar
unzipArchive :: FilePath -> FilePath -> IO()
unzipArchive archive tar = do
  putStrLn $ "Unzipping " ++ archive ++ " to " ++ tar
  unzipFile archive tar

unzipArchiveCommand :: REPLCommand HackageUpdateInfo
unzipArchiveCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "unzip",
  cExec = \iuh _ -> unzipArchive (getArchive iuh) (getTar iuh),
  cDescription = \iuh -> "unzip - unzips the " ++ getArchive iuh ++ " in the " ++ getTar iuh ++ " file"
}

unzipArchiveCloneCommand :: REPLCommand HackageUpdateInfo
unzipArchiveCloneCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-unzipclone",
  cExec = \iuh _ -> unzipArchive (getArchiveClone iuh) (getTarClone iuh),
  cDescription = \iuh -> "system-unzipclone - unzips the " ++ getArchiveClone iuh ++ " in the " ++ 
    getTarClone iuh ++ " file"
}

-- Removes gz and tar files
removeArchiveFiles :: FilePath -> FilePath -> IO()
removeArchiveFiles archive tar = do
  putStrLn $ "Removing archive files " ++ archive ++ " " ++ tar
  removeIfExists archive
  removeIfExists tar

removeArchiveFilesCommand :: REPLCommand HackageUpdateInfo
removeArchiveFilesCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "clean",
  cExec = \iuh _ -> removeArchiveFiles (getArchive iuh) (getTar iuh),
  cDescription = \iuh -> "clean - deletes the " ++ getArchive iuh ++ " and " ++ getTar iuh
}

removeArchiveFilesCloneCommand :: REPLCommand HackageUpdateInfo
removeArchiveFilesCloneCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-cleanclone",
  cExec = \iuh _ -> removeArchiveFiles (getArchiveClone iuh) (getTarClone iuh),
  cDescription = \iuh -> "system-cleanclone - deletes the " ++ 
    getArchiveClone iuh ++ " and " ++ getTarClone iuh
}

-- Compares the two gz archives. Needed to find that the archive was not incremental
showArchiveCompare :: FilePath -> FilePath -> IO()
showArchiveCompare archive1 archive2 = do
  val <- compareFiles archive1 archive2
  putStrLn $ "Compare result " ++ archive1 ++ " " ++ archive2 ++ " " ++ show val

archiveCompareCommand :: REPLCommand HackageUpdateInfo
archiveCompareCommand = RC {
  cTag = "system",
  cMatch = isTrimCommand "system-compare",
  cExec = \iuh _ -> showArchiveCompare (getArchive iuh) (getArchiveClone iuh),
  cDescription = \iuh -> "system-compare - compares the " ++ 
    getArchive iuh ++ " with " ++ getArchiveClone iuh
}

updateArchive :: URL -> URL -> FilePath -> IO UpdateResult
updateArchive = performArchiveFileUpdate

updateArchiveCommand :: REPLCommand HackageUpdateInfo
updateArchiveCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "update",
  cExec = \iuh _ -> void $ updateArchive (iuhSnapshotURL iuh) (iuhArchiveURL iuh) (getArchive iuh),
  cDescription = \iuh -> "update - updates the current " ++ getArchive iuh ++ 
    " from " ++ iuhArchiveURL iuh
}

updatePersistentFromTar :: FilePath -> FilePath -> IO()
updatePersistentFromTar updateDir tarFile = do
  newTar <- loadTar tarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  updatePersistentMap updateDir newMap 


updatePersistentFromArchiveCommand :: REPLCommand HackageUpdateInfo
updatePersistentFromArchiveCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "tarpersist",
  cExec = \iuh _ -> void $ updatePersistentFromTar (getArchivePersistDir iuh) (getTar iuh),
  cDescription = \iuh -> "tarpersist - updates the persistent storage with " ++ getTar iuh
}

showPersistentQuery :: FilePath -> PackageName -> IO()
showPersistentQuery updateDir name = do
  putStrLn $ "Querying storage hackage map with " ++ name
  value <- queryPersistentMap updateDir name
  case value of 
    Just package -> do
      putStrLn "Found"
      print package
    Nothing -> putStrLn "Not found"

showPersistentQueryCommand :: REPLCommand HackageUpdateInfo
showPersistentQueryCommand = RC {
  cTag = "hackage",
  cMatch = isPrefixCommand "querypersist ",
  cExec = \iuh command -> showPersistentQuery (getArchivePersistDir iuh) (parseValEnd command),
  cDescription = const "querypersist name - queries the persistent storage with package"
}

showPersistentTarCompare :: FilePath -> FilePath -> IO()
showPersistentTarCompare updateDir tarFile = do
  newTar <- loadTar tarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  printPersistentDiffMap updateDir newMap

showPersistentTarCompareCommand :: REPLCommand HackageUpdateInfo
showPersistentTarCompareCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "cmppersist",
  cExec = \iuh _ -> void $ showPersistentTarCompare (getArchivePersistDir iuh) (getTar iuh),
  cDescription = \iuh -> "cmppersist - compares the state of " ++ 
    getTar iuh ++ " with map from persistent storage"
}

updateAllHackageCommand :: REPLCommand HackageUpdateInfo
updateAllHackageCommand = RC {
  cTag = "hackage",
  cMatch = isTrimCommand "totalupdate",
  cExec = \iuh _ -> cExec updateArchiveCommand iuh "" >> cExec unzipArchiveCommand iuh "" >>  
                    cExec updatePersistentFromArchiveCommand iuh "",
  cDescription = const "totalupdate - updates hackage archive, unzips and updates the persistent storage"
}
