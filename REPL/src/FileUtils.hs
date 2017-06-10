{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileUtils(
                  FileSnapshotData, 
                  truncateIfExists, 
                  removeIfExists, 
                  calcFileData, 
                  compareFiles, 
                  unzipFile,
                  loadTar,
                  getFileSubstring) where

import Common

import qualified Data.Serialize as DS
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as X
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Data.Digest.Pure.MD5
import Data.Int(Int64)
import System.Posix(fileSize)
import System.Posix.Types(COff(..))
import System.Posix.Files (getFileStatus, setFileSize)
import System.IO.Error (isDoesNotExistError)
import System.Directory(removeFile, doesFileExist, createDirectoryIfMissing)
import Control.Monad(when)
import System.FilePath(takeDirectory)

-- Two type aliases for the snapshot, that is created from reading the disk file 
-- and the snapshot that is retrieved from the hackage.
type FileSnapshotData = SnapshotData

-- Calculates the file size
getFileSize :: FilePath -> IO Int64
getFileSize path = fileSize <$> getFileStatus path >>= \(COff v) -> return v

-- Calculates the snapshot of the file of the archive
calcFileData :: FilePath -> IO FileSnapshotData
calcFileData file = do 
  exists <- doesFileExist file -- does not throw anything
  if exists then do
    digest <- calcMD5 file;
    offset <- getFileSize file;
    return $ SnapshotData (show digest) offset
  else do 
    createDirectoryIfMissing True $ takeDirectory file
    return $ SnapshotData (show $ md5 "") 0

-- Calculates the MD5 hash of the file
calcMD5 :: FilePath -> IO MD5Digest
calcMD5 file = md5 <$> BL.readFile file

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
  fileBody <- BL.readFile from
  BL.appendFile to (GZip.decompress fileBody) 

loadTar :: FilePath -> IO (Tar.Entries Tar.FormatError)
loadTar file = do
  content <- BL.readFile file
  return $ Tar.read content

