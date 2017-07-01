module HackageCommands(
                        showTarElements,
                        showTarPreElements,
                        showFileSnapshot,
                        showFileSubstring,
                        showUpdateData,
                        copyArchive,
                        showDiffMap,
                        cutFile,
                        unzipArchive,
                        removeArchiveFiles,
                        showArchiveCompare,
                        updateArchive,
                        updateArchiveVoid,
                        updateTotalArchive,

                        updatePersistentFromTar,
                        showPersistentQuery,
                        showPersistentTarCompare
                        ) where

import qualified Data.Map.Strict as M
import Data.Int(Int64)
import System.Directory(copyFile)
import Control.Monad(void)

import FileUtils
import Common
import HackageArchive
import HackageUpdate

-- shows the first count elements, parsed from the tar archive
showTarElements :: FilePath -> Int -> IO ()
showTarElements path count = do
  putStrLn $ "Displaying " ++ show count ++ " entries for " ++ path
  tar <- loadTar path
  mapM_ (print.snd) $ take count $ M.toList $ buildHackageMap tar (buildPreHackageMap tar)

-- shows the first count pre elements (only path is parsed) form the tar archive
showTarPreElements :: FilePath -> Int -> IO ()
showTarPreElements path count = do
  putStrLn $ "Pre displaying " ++ show count ++ " entries for " ++ path
  tar <- loadTar path
  mapM_ print $ take count $ M.toList $ buildPreHackageMap tar

-- Displays the snapshot of the file
showFileSnapshot :: FilePath -> IO()
showFileSnapshot file = do
  filesnapshot <- calcFileData file
  putStrLn $ "File result for " ++ file
  putStrLn $ "\tFile snapshot: " ++ show filesnapshot


-- Shows the update data for the archive on disk
showUpdateData :: FilePath -> URL -> IO()
showUpdateData file json = do
  (range, snapshot, filesnapshot) <- calcUpdateResultIO file json
  putStrLn $ "Update result for file " ++ file
  putStrLn $ "\tHackage snapshot: " ++ show snapshot
  putStrLn $ "\tFile snapshot: " ++ show filesnapshot
  putStrLn $ "\tRange to update: " ++ show range

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


-- this method cuts the data from the end of the archive,
-- because hackage 01-index.tar.gz is not strictly incremental
cutFile :: FilePath -> Int64 -> IO()
cutFile path size = do
  truncateIfExists path size
  putStrLn $ "Cut " ++ show size ++ " bytes from " ++ path

-- Unzips the gz archive to tar
unzipArchive :: FilePath -> FilePath -> IO()
unzipArchive archive tar = do
  putStrLn $ "Unzipping " ++ archive ++ " to " ++ tar
  unzipFile archive tar

-- Removes gz and tar files
removeArchiveFiles :: FilePath -> FilePath -> IO()
removeArchiveFiles archive tar = do
  putStrLn $ "Removing archive files " ++ archive ++ " " ++ tar
  removeIfExists archive
  removeIfExists tar

-- Compares the two gz archives. Needed to find that the archive was not incremental
showArchiveCompare :: FilePath -> FilePath -> IO()
showArchiveCompare archive1 archive2= do
  val <- compareFiles archive1 archive2
  putStrLn $ "Compare result " ++ archive1 ++ " " ++ archive2 ++ " " ++ show val

updateArchive :: URL -> URL -> FilePath -> IO UpdateResult
updateArchive = performArchiveFileUpdate

updateArchiveVoid :: URL -> URL -> FilePath -> IO ()
updateArchiveVoid snapshotURL archiveURL archive = 
    void (performArchiveFileUpdate snapshotURL archiveURL archive)

updateTotalArchive :: IO UpdateResult -> IO() -> IO() -> IO ()
updateTotalArchive update unzip persist = do
    putStrLn "Performing total update" 
    result <- update
    if result == ArchiveIsOk then putStrLn "Nothing to update"
                             else unzip >> persist

updatePersistentFromTar :: FilePath -> FilePath -> IO()
updatePersistentFromTar updateDir tarFile = do
  newTar <- loadTar tarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  updatePersistentMap updateDir newMap 

showPersistentQuery :: FilePath -> PackageName -> IO()
showPersistentQuery updateDir name = do
  putStrLn $ "Querying storage hackage map with " ++ name
  value <- queryPersistentMap updateDir name
  case value of 
    Just package -> do
      putStrLn "Found"
      print package
    Nothing -> putStrLn "Not found"

showPersistentTarCompare :: FilePath -> FilePath -> IO()
showPersistentTarCompare updateDir tarFile = do
  newTar <- loadTar tarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  printPersistentDiffMap updateDir newMap

