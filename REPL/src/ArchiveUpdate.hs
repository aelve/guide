{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ArchiveUpdate (
  URL,
  SnapshotData(..), 
  HackageSnapshotData, 
  FileSnapshotData,
  UpdateArchiveException,
  performArchiveFileUpdate,
  getFileSubstring,
  calcFileData, 
  calcUpdateResult2,
  truncateIfExists,
  unzipFile,
  removeIfExists,
  compareFiles) where

import Network.HTTP.Client(Request(..), parseUrlThrow, newManager, responseBody, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Text as T
import qualified Data.Char as DC
import qualified Data.List as DL
import Data.Aeson.Types

import Data.Digest.Pure.MD5
import qualified Data.Serialize as DS

import Data.Int(Int64)
import qualified Control.Exception as X
import System.IO.Error (isDoesNotExistError)
import System.Posix(fileSize)
import System.Posix.Types(FileOffset, COff(..))
import System.Posix.Files (getFileStatus, setFileSize)
import System.Directory(removeFile, doesFileExist, copyFile, createDirectoryIfMissing)
import System.FilePath(takeDirectory)
import Control.Monad(when, forever)
import qualified Codec.Compression.GZip as GZip


data SnapshotData = SnapshotData { 
  md5Hash :: String,
  lengthFile :: Int64
} deriving (Eq, Show)


-- Two type aliases for the snapshot, that is created from reading the disk file 
-- and the snapshot that is retrieved from the hackage.
type HackageSnapshotData = SnapshotData
type FileSnapshotData = SnapshotData

-- Snapshot aeson construction instance
instance FromJSON SnapshotData where
  parseJSON = withObject "snapshot" $ \o -> do
    signedO <- o .: "signed"
    metaO <- signedO .: "meta"
    tarO <- metaO .: "<repo>/01-index.tar.gz"
    hashesO <- tarO .: "hashes"
    md5str <- hashesO .: "md5"
    len <- tarO .: "length"
    return (SnapshotData md5str len)

-- The exception, that is raised, when there is problems with creating the
-- snapshot
newtype UpdateArchiveException = UAE String deriving (Show, Eq)
instance X.Exception UpdateArchiveException

-- The method, that raises an exception, if it was not able to parse the
-- snapshot from JSON
parseSnapshotJSONThrow :: BL.ByteString -> IO SnapshotData
parseSnapshotJSONThrow body = case A.decode body of 
  (Just snapshot) -> return snapshot
  Nothing -> X.throwIO $ UAE "Could not decode JSON"

-- Alias for URL address. Just to make the code more pleasant
type URL = String

-- The range, from which to download 
type Range = (Int64, Int64)

-- Chops the range into the list of ranges, for adequate downloading
cropRanges :: Int64 -> Range -> [Range]
cropRanges maxRange (from, to) 
 | to - from + 1 <= maxRange = [(from, to)]
 | otherwise = (from, from + maxRange - 1) : cropRanges maxRange (from + maxRange, to)

-- Creates the request by parsing url and then modifies it to make range request
createRangeRequest :: URL -> Range -> IO Request
createRangeRequest url range = makeRangeRequest range <$> parseUrlThrow url

-- Writes the range to the simple http request
makeRangeRequest :: Range -> Request -> Request
makeRangeRequest (from, to) = makeRange
  where 
    br = ByteRangeFromTo (fromIntegral from) (fromIntegral to)
    makeRange r = r {
      requestHeaders = (hRange, renderByteRanges [br]) : requestHeaders r
    }

-- Returns the data from response, returned to the request
fetchResponseData :: Request -> IO BL.ByteString
fetchResponseData req = newManager tlsManagerSettings >>= httpLbs req >>= return.responseBody

-- Returns the snapshot of archive from the hackage
fetchSnapshot :: URL -> IO SnapshotData
fetchSnapshot url = parseUrlThrow url >>= fetchResponseData >>= parseSnapshotJSONThrow

-- Returns the bytes from the range request
fetchRangeData :: URL -> Range -> IO BL.ByteString
fetchRangeData url range = createRangeRequest url range >>= fetchResponseData

-- Calculates the MD5 hash of the file
calcMD5 :: FilePath -> IO MD5Digest
calcMD5 file = BL.readFile file >>= return.md5

-- Calculates the file size
getFileSize :: String -> IO Int64
getFileSize path = getFileStatus path >>= return.fileSize >>= \(COff v) -> return v

-- Calculates the snapshot of the file of the archive
calcFileData :: FilePath -> IO SnapshotData
calcFileData file = do 
  exists <- doesFileExist file -- does not throw anything
  if exists then do
    digest <- calcMD5 file;
    offset <- getFileSize file;
    return $ SnapshotData (show digest) offset
  else do 
    createDirectoryIfMissing True $ takeDirectory file
    return $ SnapshotData (show $ md5 "") 0

-- The action, that is needed to perform to correctly update the downloaded
-- archive. ArchiveIsOk - everything is fine.
-- Update - need to add some information to the end of the file
-- Reload - need to redownload the whole archive completely
data UpdateRange = ArchiveIsOk | Corrupted | Update Range deriving (Eq, Show)


-- The maximum range to download in one request from the hackage
maxRange :: Int64
maxRange = 512000

-- Calculates the update result of the current archive using two snapshots
calcUpdateResult :: HackageSnapshotData -> FileSnapshotData -> UpdateRange
calcUpdateResult hackage file 
  | hackage == file = ArchiveIsOk -- both are equal
  | lenH > lenF = Update (lenF, lenH - 1) -- need to append a bit
  | otherwise = Corrupted -- delete old file and redownload it
  where lenH = lengthFile hackage
        lenF = lengthFile file

-- Calculates the update range in the IO monad
-- I didn't know how to name this method, so just added 2 to the end
calcUpdateResult2 :: FilePath -> URL -> IO (UpdateRange, HackageSnapshotData, FileSnapshotData)
calcUpdateResult2 file json = do
  snapshot <- fetchSnapshot json
  fileData <- calcFileData file
  return (calcUpdateResult snapshot fileData, snapshot, fileData)


-- Deletes the file it it exists. 
removeIfExists :: FilePath -> IO ()
removeIfExists file = removeFile file `X.catch` exhandler
  where exhandler e | isDoesNotExistError e = return ()
                    | otherwise = X.throwIO e

-- Cuts the end of the file, in case it exists and the amount of bytes to cut is
-- less than file's length
truncateIfExists :: FilePath -> Int64 -> IO ()
truncateIfExists file amount = do
  fileData <- calcFileData file  
  when (lengthFile fileData - amount > 0) $ setFileSize file $ COff (lengthFile fileData - amount)


-- compares two files and returns the byte number, when they start to differ
-- It it used to check, where the archive and the updated archive differ
compareFiles :: FilePath -> FilePath -> IO Int64
compareFiles file1 file2 = do
  c1 <- BL.readFile file1
  c2 <- BL.readFile file2 
  return $ compareFunc 0 c1 c2
  where
    compareFunc :: Int64 -> BL.ByteString -> BL.ByteString -> Int64
    compareFunc ind bstr1 bstr2 
      | BL.null bstr1 && BL.null bstr2 = -1 -- the strings are equal
      | BL.null bstr1 || BL.null bstr2 = ind -- one string is empty so the diff is on ind byte
      | BL.head bstr1 /= BL.head bstr2 = ind -- the byte is not equal
      | otherwise = compareFunc (ind + 1) (BL.tail bstr1) (BL.tail bstr2)

-- Returns the byte substring from file
getFileSubstring :: FilePath -> Int64 -> Int64 -> IO BL.ByteString
getFileSubstring file from len  = do
  c <- BL.readFile file
  return $ BL.take len $ BL.drop from c

-- unzips the file to the other file
unzipFile :: FilePath -> FilePath -> IO()
unzipFile from to = do
  removeIfExists to
  fileBody <- (BL.readFile from)
  BL.appendFile to (GZip.decompress fileBody) 


-- performs the update, returns True if the the archive was modified



performArchiveFileUpdate :: URL -> URL -> FilePath -> IO UpdateRange
performArchiveFileUpdate snapshotURL archiveURL archive = do
  putStrLn $ "Updating " ++ archive ++ " from " ++ archiveURL
  (status, snapshot, _) <- calcUpdateResult2 archive snapshotURL

  case status of 
    ArchiveIsOk ->  (putStrLn $ "Archive is up to date") >> return ArchiveIsOk
    _ -> cutUpdate modifFunctions
  where 
    performUpdate = updateArchive archive archiveURL
    modifFunctions = [return (), cutting 50000, cutting 500000, cutting 5000000, removing]    
    cutting val = do
      putStrLn $ "\tCutting " ++ (show val) ++ " from " ++ archive
      truncateIfExists archive val
    removing = do
      putStrLn $ "\tRemoving " ++ archive
      removeIfExists archive

    cutUpdate (mf : mfs) = do
      mf
      (status, snapshot, _) <- calcUpdateResult2 archive snapshotURL
      case status of 
        ArchiveIsOk -> return ArchiveIsOk
        Corrupted -> cutUpdate mfs 
        Update range -> do
          putStrLn $ "\tSnapshot from " ++ snapshotURL ++ " " ++ (show snapshot)
          putStrLn $ "\tUpdate range " ++ (show range)
          result <- performUpdate snapshot range
          if result then return status
                    else cutUpdate mfs
    cutUpdate [] = do
      putStrLn "Failed to update"
      return Corrupted

updateArchive :: FilePath -> URL -> HackageSnapshotData -> Range -> IO Bool
updateArchive archive archiveURL snapshot range = do
  mapM_  (write2File archive archiveURL) (cropRanges maxRange range)
  newFileData <- calcFileData archive
  return (newFileData == snapshot)

write2File :: FilePath -> URL -> Range -> IO() 
write2File archive url range = do
  putStrLn $ "\tGetting range " ++ (show range) ++ " from " ++ url
  body <- fetchRangeData url range
  putStrLn $ "\tGot range " ++ (show (BL.take 50 body))
  BL.appendFile archive body
  putStrLn "Append ok"

{-
performArchiveCutUpdateF :: (FilePath -> IO Bool) -> FilePath -> Int64 -> IO Bool
performArchiveCutUpdateF updateFunc archive cutSize = do
  putStrLn $ "Cutting " ++ (show cutSize) ++ " from " ++ archive ++ " before update"
  truncateIfExists archive cutSize
  updateFunc archive

performArchiveCutUpdate :: URL -> URL -> FilePath -> Int64 -> IO Bool
performArchiveCutUpdate snapshotURL archiveURL = performArchiveCutUpdateF (performArchiveFileUpdate snapshotURL archiveURL)
-}

  
  {-
  case range of 
    ArchiveIsOk -> (putStrLn $ "Archive is up to date") >> return ArchiveIsOk
    Update range -> do 
      putStrLn $ "Updating " ++ archive ++ " from " ++ archiveURL
      result <- updateArchive archive archiveURL snapshot range
      putStrLn $ if result then "Update successfull" else "MD5 does not match"
      return ArchiveIsOk
   -}   
    {-
    Corrupted -> do
      putStrLn $ "Reloading " ++ archive ++ " from " ++ archiveURL
      removeIfExists archive
      result <- updateArchive archive archiveURL snapshot range
      putStrLn $ if result then "Update successfull" else "MD5 does not match"
      return True
    -}
    
 
