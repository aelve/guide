{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module REPL ( processCycle,
              updateArchive,
              updateMapFromTar,
              queryHackageMap,
              HackageUpdateInfo (..)
              ) where 

import qualified Data.Map.Strict as M
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Control.Exception as X
import Control.Monad(forever)
import System.IO (stdout, hFlush)
import Data.Default

import Data.Int(Int64)
import System.Exit(exitSuccess)
import System.Directory(copyFile)
import System.FilePath((</>))

import Common
import HackageArchive
import HackageUpdate
import FileUtils
import HttpDownload
import Storage

-- the constructor short name is really awkward in russian
data HackageUpdateInfo = IUH { 
  iuhUpdateDir :: FilePath,
  iuhSnapshotURL :: URL,
  iuhArchiveURL :: URL
} deriving (Eq, Show)

instance Default HackageUpdateInfo where
  def = defaultIUH

defaultIUH :: HackageUpdateInfo
defaultIUH = IUH {
  iuhUpdateDir = "hackagefiles",
  iuhSnapshotURL = "https://hackage.haskell.org/snapshot.json",
  iuhArchiveURL = "https://hackage.haskell.org/01-index.tar.gz"
}

getArchive :: HackageUpdateInfo -> FilePath
getArchive iuh = (iuhUpdateDir iuh) </> archive

getArchiveClone :: HackageUpdateInfo -> FilePath
getArchiveClone iuh = (iuhUpdateDir iuh) </> archiveClone


getTar :: HackageUpdateInfo -> FilePath
getTar iuh = (iuhUpdateDir iuh) </> tar

getTarClone :: HackageUpdateInfo -> FilePath
getTarClone iuh = (iuhUpdateDir iuh) </> tarClone

archive :: FilePath
archive = "01-index.tar.gz"

archiveClone :: FilePath
archiveClone = "01-index.tar.gz.orig"

tar :: FilePath
tar = "01-index.tar"

tarClone :: FilePath
tarClone = "01-index.orig.tar" 

parseIntEnd :: (Num a, Read a) => String -> a
parseIntEnd val | DL.length l > 0 = read (DL.last l)
                | otherwise = 0
                where l = words val

parseValEnd :: String -> String
parseValEnd val | DL.length l > 1 = DL.last l
                | otherwise = ""
                where l = words val

processCycle :: HackageUpdateInfo -> IO ()
processCycle iuh = forever $ do
  putStr "Input command: "
  hFlush stdout
  command <- getLine
  hFlush stdout
  (processCommand command) `X.catch` eh `X.catch` eh2 `X.catch` eh3
  where 
    processCommand = buildCommand iuh
    eh (e :: X.IOException) = putStrLn $ "IO Error: " ++ (show e)
    eh2 (e :: UpdateArchiveException) = putStrLn $ "Parsing error: " ++ (show e) 
    eh3 (e :: X.ErrorCall) = putStrLn $ "Error call: " ++ (show e)


buildCommand :: HackageUpdateInfo -> (String -> IO())
buildCommand iuh = processCommand
  where 
    processCommand command
      -- checks the current gzip archive and understands what to download
      | chk "checkclone" = showUpdateData archC snapURL
      -- checks the current gzip archive and understands what to download
      | chk "check" = showUpdateData arch snapURL     

      | chk "fileclone" = showFileSnapshot archC
      | chk "file" = showFileSnapshot arch  -- shows the snapshot of hackage file
      
      | chk "copyorig" = copyArchive arch archC -- copies the current archive to the orig place

      | chk "cutclone" = cutFile archC (parseIntEnd command) 
      | chk "cut" = cutFile arch (parseIntEnd command) -- cuts the end of the gzip file for checking purposes

      | chk "unzipclone" = unzipArchive archC trFileC -- unzips the downloaded gzip archive
      | chk "unzip" = unzipArchive arch trFile  -- unzips the downloaded gzip archive

      | chk "cleanclone" = removeArchiveFiles archC trFileC
      | chk "clean" = removeArchiveFiles arch trFile

      | chk "tarparsepreclone" = showPreMap trFileC 50 -- loads the tar clone information in the memory
      | chk "tarparsepre" = showPreMap trFile 50  -- loads the tar information in the memory

      | chk "tarparseclone" = showMap trFileC 50 -- loads the tar clone information in the memory
      | chk "tarparse" = showMap trFile 50  -- loads the tar information in the memory

      | chk "compare" = showArchiveCompare arch archC 
      | chk "update" = performArchiveFileUpdate snapURL archURL arch >> return ()

      | chk "acidcompare" = printAcidCompare ud trFile
      | chk "acidupdate" = acidUpdate ud trFile
      | chk "acidquery" =  printAcidQuery ud (parseValEnd command)

      | chk "tarcmp" = showDiffMap trFile trFileC
      | chk "exit" = exitREPL

      | chk "help" = showHelp iuh
      | otherwise = showHelp iuh

      where pc = map DC.toLower command
            chk val = DL.isPrefixOf val pc

            arch = getArchive iuh
            archC = getArchiveClone iuh 
            archURL = iuhArchiveURL iuh
            snapURL = iuhSnapshotURL iuh
            trFile = getTar iuh
            trFileC = getTarClone iuh
            ud = iuhUpdateDir iuh


-- Displays the snapshot of the file
showFileSnapshot :: FilePath -> IO()
showFileSnapshot file = do
  filesnapshot <- calcFileData file
  putStrLn $ "File result for " ++ file
  putStrLn $ "\tFile snapshot: " ++ (show filesnapshot)

-- Shows the update data for the archive on disk
showUpdateData :: FilePath -> URL -> IO()
showUpdateData file json = do
  (range, snapshot, filesnapshot) <- calcUpdateResultIO file json
  putStrLn $ "Update result for file " ++ file
  putStrLn $ "\tHackage snapshot: " ++ (show snapshot)
  putStrLn $ "\tFile snapshot: " ++ (show filesnapshot)
  putStrLn $ "\tRange to update: " ++ (show range)

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

showMap :: FilePath -> Int -> IO()
showMap path count = do
  putStrLn $ "Displaying " ++ (show count) ++ " entries for " ++ path
  tar <- loadTar path
  mapM_ (print.snd) $ take count $ M.toList $ buildHackageMap tar (buildPreHackageMap tar)

showPreMap :: FilePath -> Int -> IO()
showPreMap path count = do
  putStrLn $ "Pre displaying " ++ (show count) ++ " entries for " ++ path
  tar <- loadTar path
  mapM_ print $ take count $ {-filter ((elem '-').fst) $-} M.toList $ buildPreHackageMap tar


showDiffMap :: FilePath -> FilePath -> IO ()
showDiffMap newTarFile oldTarFile = do
  putStrLn $ "Displaying difference between " ++ newTarFile ++ " and " ++ oldTarFile
  newTar <- loadTar newTarFile
  oldTar <- loadTar oldTarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  let oldMap = buildHackageMap oldTar (buildPreHackageMap oldTar)
  let diffMap = buildDifferenceMap oldMap newMap
  mapM_ (print.snd) $ M.toList diffMap

showHelp :: HackageUpdateInfo -> IO()
showHelp iuh = do
  putStrLn "Available commands: "

  putStrLn $ "check - downloads the json length and md5 hash from " ++ snapURL ++ 
             ", and compares it with local " ++ arch
  putStrLn $ "checkclone - same for " ++ archC
  putStrLn $ "file - displays the current " ++ arch ++ " length and md5 hash"
  putStrLn $ "fileclone - same for " ++ archC ++ " file"
  putStrLn $ "copyorig - copy the " ++ arch ++ " to " ++ archC
  putStrLn $ "cut size - cuts the size bytes from the end of the " ++ arch ++ " , for update command"
  putStrLn $ "cutclone size - cuts the size bytes from the end of the 01-index.tar.gz, for update command"
  putStrLn $ "unzip - unzips the " ++ arch ++ " in the " ++ trFile ++ " file"
  putStrLn $ "unzipclone - unzips the " ++ archC ++ " in the " ++ trFileC ++ " file"
  putStrLn $ "clean - deletes the " ++ arch ++ " and " ++ trFile
  putStrLn $ "cleanclone - deletes the " ++ archC ++ " and " ++ trFileC
  putStrLn $ "compare - compares the " ++ arch ++ " with " ++ archC
  putStrLn $ "tarparse - loads the map of entries from " ++ trFile ++ " and displays it" 
  putStrLn $ "tarparseclone - same for " ++ trFileC
  putStrLn $ "tarparsepre - loads the  premap of entries from " ++ trFile ++ " and displays it" 
  putStrLn $ "tarparsepreclone - same for " ++ trFileC
  putStrLn $ "tarcmp - compares the entries of " ++ trFile ++ " and " ++ trFileC
  putStrLn $ "update - updates the current " ++ arch ++ " from " ++ archURL
  putStrLn $ "acidcompare - compares the state of " ++ trFile ++ " with map from acid state"
  putStrLn $ "acidupdate - updates the acid state with " ++ trFile
  putStrLn $ "acidquery name - queries the acid with package"
  putStrLn "exit - exits this repl"

  where 
    arch = getArchive iuh
    archC = getArchiveClone iuh 
    archURL = iuhArchiveURL iuh
    snapURL = iuhSnapshotURL iuh
    trFile = getTar iuh
    trFileC = getTarClone iuh

showArchiveCompare :: FilePath -> FilePath -> IO()
showArchiveCompare archive1 archive2= do
  val <- compareFiles archive1 archive2
  putStrLn $ "Compare result " ++ archive1 ++ " " ++ archive2 ++ " " ++ (show val)

exitREPL :: IO()
exitREPL = putStrLn "Finished working with hackage REPL" >> exitSuccess

-- this method cuts the data from the end of the archive
-- needed mostly for testing purposes
cutFile :: FilePath -> Int64 -> IO()
cutFile path size = do
  truncateIfExists path size
  putStrLn $ "Cut " ++ (show size) ++ " bytes from " ++ path

unzipArchive :: FilePath -> FilePath -> IO()
unzipArchive archive tar = do
  putStrLn $ "Unzipping " ++ archive ++ " to " ++ tar
  unzipFile archive tar

removeArchiveFiles :: FilePath -> FilePath -> IO()
removeArchiveFiles archive tar = do
  putStrLn $ "Removing archive files " ++ archive ++ " " ++ tar
  removeIfExists archive
  removeIfExists tar


printAcidCompare :: FilePath -> FilePath -> IO()
printAcidCompare updateDir tarFile = do
  newTar <- loadTar tarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  printAcidDiffMap updateDir newMap

acidUpdate :: FilePath -> FilePath -> IO()
acidUpdate updateDir tarFile = do
  newTar <- loadTar tarFile
  let newMap = buildHackageMap newTar (buildPreHackageMap newTar)
  updateAcidMap updateDir newMap 

printAcidQuery :: FilePath -> HackageName -> IO()
printAcidQuery updateDir name = do
  putStrLn $ "Querying acid with " ++ name
  value <- queryAcidMap updateDir name
  case value of 
    Just package -> do
      putStrLn "Found"
      print package
    Nothing -> putStrLn "Not found"

updateArchive :: HackageUpdateInfo -> IO()
updateArchive iuh = performArchiveFileUpdate snapURL archURL arch >> return ()
  where 
    arch = getArchive iuh
    archURL = iuhArchiveURL iuh
    snapURL = iuhSnapshotURL iuh

updateMapFromTar :: HackageUpdateInfo -> IO()
updateMapFromTar iuh = acidUpdate (iuhUpdateDir iuh) (getTar iuh)

queryHackageMap :: HackageUpdateInfo -> HackageName -> IO (Maybe HackagePackage)
queryHackageMap iuh = queryAcidMap (iuhUpdateDir iuh) 







