{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module REPL ( processREPLCycle ) where 

import qualified Data.Char as DC
import qualified Control.Exception as X

import Data.List(isPrefixOf)
import Control.Monad(forever, void)
import System.IO (stdout, hFlush)
import System.Exit(exitSuccess)

import Common
import qualified HackageCommands as HC
import qualified StackageCommands as SC

processREPLCycle :: UpdateInfo -> IO ()
processREPLCycle ui = forever $ do
  putStr "Input command: "
  hFlush stdout
  command <- getLine
  hFlush stdout
  processCommand command `X.catch` eh `X.catch` eh2 `X.catch` eh3
  where 
    processCommand = buildCommand ui
    eh (e :: X.IOException) = putStrLn $ "IO Error: " ++ show e
    eh2 (e :: UpdateArchiveException) = putStrLn $ "Parsing error: " ++ show e 
    eh3 (e :: X.ErrorCall) = putStrLn $ "Error call: " ++ show e

buildCommand :: UpdateInfo -> (String -> IO())
buildCommand ui = processCommand
  where 
    processCommand command
      -- checks the current hackage gzip archive and understands what to download
      | chk "check" = HC.showUpdateData arch snapURL     
      -- updates the gzip archive file, unpacks it to tar and loads in the permanent storage
      | chk "totalupdate" = HC.updateTotalArchive updateCommand unzipCommand persistCommand
      -- updates the gzip archive file from hackage
      | chk "update" = HC.updateArchiveVoid snapURL archURL arch 
      -- shows the snapshot of hackage gzip archive file (md5 and length)
      | chk "file" = HC.showFileSnapshot arch  
      -- cuts the end of the hackage gzip archive file for checking purposes
      | chk "cut" = HC.cutFile arch (parseIntEnd command) 
      -- unzips the downloaded gzip archive to tar file
      | chk "unzip" = HC.unzipArchive arch trFile  
      -- removes the gzip and tar files
      | chk "clean" = HC.removeArchiveFiles arch trFile
      -- shows the first 50 pre elements from tar archive
      | chk "tarshowpre" = HC.showTarPreElements trFile 50  
      -- shows the first 50 elements from tar archive
      | chk "tarshow" = HC.showTarElements trFile 50 
      -- Updates the persistent map from tar archive
      | chk "tarpersist" = persistCommand
      -- compares the map from tar archive and the persistent map
      | chk "cmppersist" = HC.showPersistentTarCompare ud trFile
      -- shows the package from the persistent map 
      | chk "querypersist" =  HC.showPersistentQuery ud (parseValEnd command)

      -- shows the snapshots from stackage
      | chk "snapshots" = SC.showSnapshots snapshotsURL
      -- exits the REPL
      | chk "exit" = exitREPL
      | chk "quit" = exitREPL

      -- shows the help for REPL commands
      | chk "help" = showHelp ui

      -- these are the clones of the commands above for the orig files
      -- You'll probably won't need them, unless you are me (borboss366)
      -- copies the current hackage archive to other file. Needed for checking properties
      | chk "system-copyorig" = HC.copyArchive arch archC 
      | chk "system-checkclone" =  HC.showUpdateData archC snapURL
      | chk "system-fileclone" = HC.showFileSnapshot archC
      | chk "system-cutclone" = HC.cutFile archC (parseIntEnd command) 
      | chk "system-unzipclone" = HC.unzipArchive archC trFileC 
      | chk "system-cleanclone" = HC.removeArchiveFiles archC trFileC
      | chk "system-tarshowpreclone" = HC.showTarPreElements trFileC 50 
      | chk "system-tarshowclone" = HC.showTarElements trFileC 50
      -- compares the gzip archive with orig archive, that was copied some time before
      | chk "system-compare" = HC.showArchiveCompare arch archC 
      -- shows diff map between tar and tar.orig archives
      | chk "system-tarcmp" = HC.showDiffMap trFile trFileC

      | otherwise = showHelp ui

      where pc = map DC.toLower command
            chk val = val `isPrefixOf` pc

            arch = (getArchive.iuh) ui
            archC = (getArchiveClone.iuh) ui
            archURL = (iuhArchiveURL.iuh) ui
            snapURL = (iuhSnapshotURL.iuh) ui
            trFile = (getTar.iuh) ui
            trFileC = (getTarClone.iuh) ui
            ud = (iuhUpdateDir.iuh) ui

            snapshotsURL = (getSnapshotURL.sui) ui

            updateCommand = HC.updateArchive snapURL archURL arch 
            unzipCommand = HC.unzipArchive arch trFile 
            persistCommand = HC.updatePersistentFromTar ud trFile


showHelp :: UpdateInfo -> IO()
showHelp ui = do
  putStrLn "Available commands: "

  putStrLn $ "check - downloads the json length and md5 hash from " ++ snapURL ++ 
             ", and compares it with local " ++ arch
  putStrLn $ "file - displays the current " ++ arch ++ " length and md5 hash"
  putStrLn $ "cut size - cuts the size bytes from the end of the " ++ arch ++ " , for update command"
  putStrLn $ "unzip - unzips the " ++ arch ++ " in the " ++ trFile ++ " file"
  putStrLn $ "clean - deletes the " ++ arch ++ " and " ++ trFile
  putStrLn $ "update - updates the current " ++ arch ++ " from " ++ archURL
  putStrLn $ "totalupdate - updates the current " ++ arch ++ " from " ++ archURL
  putStrLn $ "tarshow - loads the map of entries from " ++ trFile ++ " and displays it" 
  putStrLn $ "tarshowpre - loads the  premap of entries from " ++ trFile ++ " and displays it" 
  putStrLn $ "cmppersist - compares the state of " ++ trFile ++ " with map from persistent storage"
  putStrLn $ "tarpersist - updates the persistent storage with " ++ trFile
  putStrLn "querypersist name - queries the persistent storage with package"

  putStrLn $ "snapshots - show the stackage snapshots from " ++ snapshotsURL

  putStrLn "exit - exits this repl"
  putStrLn "help - shows this help"

{-
  putStrLn $ "compare - compares the " ++ arch ++ " with " ++ archC
  putStrLn $ "tarcmp - compares the entries of " ++ trFile ++ " and " ++ trFileC

  putStrLn $ "acidcmp - compares the state of " ++ trFile ++ " with map from acid state"
  putStrLn $ "acidupdate - updates the acid state with " ++ trFile
  putStrLn "acidquery name - queries the acid with package"
  putStrLn $ "checkclone - same for " ++ archC
    putStrLn $ "fileclone - same for " ++ archC ++ " file"
  putStrLn $ "copyorig - copy the " ++ arch ++ " to " ++ archC
  putStrLn "cutclone size - cuts the size bytes from the end of the 01-index.tar.gz, for update command"
  putStrLn $ "unzipclone - unzips the " ++ archC ++ " in the " ++ trFileC ++ " file"
  putStrLn $ "cleanclone - deletes the " ++ archC ++ " and " ++ trFileC
-}
  where 
    arch = (getArchive.iuh) ui
    archC = (getArchiveClone.iuh) ui
    archURL = (iuhArchiveURL.iuh) ui
    snapURL = (iuhSnapshotURL.iuh) ui
    trFile = (getTar.iuh) ui
    trFileC = (getTarClone.iuh) ui
    snapshotsURL = (getSnapshotURL.sui) ui


exitREPL :: IO()
exitREPL = putStrLn "Finished working with hackage REPL" >> exitSuccess