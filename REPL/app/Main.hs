{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Int(Int64)
import qualified Control.Exception as X
import qualified Data.Char as DC
import qualified Data.List as DL
import Control.Monad(forever)
import System.Directory(copyFile)
import System.IO (stdout, hFlush)
import qualified Data.Map.Strict as Map

import ArchiveUpdate
import TarUtil 
import REPL
{-
updateHackage :: IO()
updateHackage = do
  val <- performSmartUpdate archiveFile snapshotURL archiveURL
  putStrLn $ "Updated " ++ show val

-- Compares the hackage archive file with the original file
compareArchive :: FilePath -> FilePath -> IO()
compareArchive archive1 archive2= do
  val <- compareFiles archive1 archive2
  putStrLn $ "Compare result " ++ archive1 ++ " " ++ archive2 ++ " " ++ (show val)

-- Parses the integer value at the end of the string
-- Used to parse commands like "cut 42"
parseIntEnd :: (Num a, Read a) => String -> a
parseIntEnd val | DL.length l > 0 = read (DL.last l)
                | otherwise = 0
  where l = words val

processCommand :: String -> IO()
processCommand command
  | chk "check" = showUpdateData archiveFile snapshotURL -- checks the current gzip archive and understands what to download
  | chk "checkclone" = showUpdateData archiveCloneFile snapshotURL -- checks the current gzip archive and understands what to download

  | chk "file" = showFileSnapshot archiveFile  -- shows the snapshot of hackage file
  | chk "fileclone" = showFileSnapshot archiveCloneFile
  | chk "copyorig" = copyArchive archiveFile archiveCloneFile -- copies the current archive to the orig place
 
  | chk "cut" = cutFile archiveFile (parseIntEnd command) -- cuts the end of the gzip file for checking purposes
  | chk "cutclone" = cutFile archiveCloneFile (parseIntEnd command) 

  | chk "unzip" = unzipArchive archiveFile tarArchive  -- unzips the downloaded gzip archive
  | chk "unzipclone" = unzipArchive archiveCloneFile tarArchiveClone -- unzips the downloaded gzip archive

  | chk "tarparse" = showMap tarArchive 50  -- loads the tar information in the memory
  | chk "tarparseclone" = showMap tarArchiveClone 50 -- loads the tar clone information in the memory
  
  | chk "tarshow" = showTarContents tarArchive
  | chk "tarshowclone" = showTarContents tarArchiveClone

  | chk "compare" = showArchiveCompare archiveFile archiveCloneFile 
  | chk "update" = updateHackage -- updates the current gzip archive
  
  | chk "tarcmp" = showDiffMap tarArchive tarArchiveClone
  | chk "exit" = exitREPL

  | chk "help" = showHelp
  | otherwise = showHelp
  where pc = map DC.toLower command
        chk val = DL.isPrefixOf val pc

processCycle :: IO ()
processCycle = forever $ do
  putStr "Input command: "
  hFlush stdout
  command <- getLine
  hFlush stdout
  (processCommand command) `X.catch` eh `X.catch` eh2 `X.catch` eh3
  where 
    eh (e :: X.IOException) = putStrLn $ "IO Error: " ++ (show e)
    eh2 (e :: UpdateArchiveException) = putStrLn $ "Parsing error: " ++ (show e) 
    eh3 (e :: X.ErrorCall) = putStrLn $ "Error call: " ++ (show e)
-}

defaultPBI :: ProcessBuilderInfo 
defaultPBI = PBI {
  archiveURL = "https://hackage.haskell.org/01-index.tar.gz",
  snapshotURL = "https://hackage.haskell.org/snapshot.json",
  archive = "01-index.tar.gz",
  archiveClone = "01-index.tar.gz.orig",
  tar = "01-index.tar",
  tarClone = "01-index.orig.tar" }

main :: IO ()
main = processCycle defaultPBI
