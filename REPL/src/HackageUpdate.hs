{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HackageUpdate (
                      performArchiveFileUpdate, 
                      calcUpdateResultIO, 
                      UpdateResult(..)) where

import Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Text as T
import qualified Control.Exception as X
import Network.HTTP.Client(parseUrlThrow)

import qualified Data.ByteString.Lazy as BL
import Data.Int(Int64)

import HttpDownload
import FileUtils
import Common

type HackageSnapshotData = SnapshotData

-- This is the data that is extracted from the path to cabal file
-- Like, when program parses "safeio/0.0.2.0/safeio.cabal"
-- It gets the version 0.0.2.0 and safeio package name. Also checks, xxx and yyy match in 
-- "xxx/version/yyy.cabal
instance FromJSON SnapshotData where
  parseJSON = withObject "snapshot" $ \o -> do
    signedO <- o .: "signed"
    metaO <- signedO .: "meta"
    tarO <- metaO .: "<repo>/01-index.tar.gz"
    hashesO <- tarO .: "hashes"
    md5str <- hashesO .: "md5"
    len <- tarO .: "length"
    return (SnapshotData md5str len)

-- The action, that is needed to perform to correctly update the downloaded
-- archive. ArchiveIsOk - everything is fine.
-- Update - need to add some information to the end of the file
-- Reload - need to redownload the whole archive completely
data UpdateResult = ArchiveIsOk | Corrupted | Update Range deriving (Eq, Show)

-- The maximum range to download in one request from the hackage
maxRange :: Int64
maxRange = 512000

-- Calculates the update result of the current archive using two snapshots
calcUpdateResult :: HackageSnapshotData -> FileSnapshotData -> UpdateResult
calcUpdateResult hackage file 
  | hackage == file = ArchiveIsOk -- both are equal
  | lenH > lenF = Update (lenF, lenH - 1) -- need to append a bit
  | otherwise = Corrupted -- the file is of desired length, but the md5 does not match
  where lenH = lengthFile hackage
        lenF = lengthFile file

-- Calculates the update range in the IO monad
-- I didn't know how to name this method, so just added 2 to the end
calcUpdateResultIO :: FilePath -> URL -> IO (UpdateResult, HackageSnapshotData, FileSnapshotData)
calcUpdateResultIO file json = do
  snapshot <- fetchSnapshot json
  fileData <- calcFileData file
  return (calcUpdateResult snapshot fileData, snapshot, fileData)


-- The method, that raises an exception, if it was not able to parse the
-- snapshot from JSON
parseSnapshotJSONThrow :: BL.ByteString -> IO HackageSnapshotData
parseSnapshotJSONThrow body = case A.decode body of 
  (Just snapshot) -> return snapshot
  Nothing -> X.throwIO $ UAE "Could not decode hackage JSON"

-- Returns the snapshot of archive from the hackage
fetchSnapshot :: URL -> IO HackageSnapshotData
fetchSnapshot url = parseUrlThrow url >>= fetchResponseData >>= parseSnapshotJSONThrow


-- performs the update, returns True if the the archive was modified
performArchiveFileUpdate :: URL -> URL -> FilePath -> IO UpdateResult
performArchiveFileUpdate snapshotURL archiveURL archive = do
  putStrLn $ "Updating " ++ archive ++ " from " ++ archiveURL
  (status, snapshot, _) <- calcUpdateResultIO archive snapshotURL

  case status of 
    ArchiveIsOk -> putStrLn "Archive is up to date" >> return ArchiveIsOk
    _ -> cutUpdate modifFunctions
  where 
    performUpdate = updateArchive archive archiveURL
    modifFunctions = [return (), cutting 50000, cutting 500000, cutting 5000000, removing]    
    cutting val = do
      putStrLn $ "\tCutting " ++ show val ++ " from " ++ archive
      truncateIfExists archive val
    removing = do
      putStrLn $ "\tRemoving " ++ archive
      removeIfExists archive

    cutUpdate (mf : mfs) = do
      mf
      (status, snapshot, _) <- calcUpdateResultIO archive snapshotURL
      case status of 
        ArchiveIsOk -> return ArchiveIsOk
        Corrupted -> cutUpdate mfs 
        Update range -> do
          putStrLn $ "\tSnapshot from " ++ snapshotURL ++ " " ++ show snapshot
          putStrLn $ "\tUpdate range " ++ show range
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
  putStrLn $ "\tGetting range " ++ show range ++ " from " ++ url
  body <- fetchRangeData url range
  putStrLn $ "\tGot range " ++ show (BL.take 50 body)
  BL.appendFile archive body
  putStrLn "Append ok"

