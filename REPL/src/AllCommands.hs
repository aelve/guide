module AllCommands(totalUpdate) where

import Common
import qualified HackageCommands as HC
import qualified StackageCommands as SC
import qualified HackageArchive as HA

totalUpdate :: UpdateInfo -> IO()
totalUpdate ui = do
    putStrLn "Total update of a system!"

    putStrLn "Stackage update..."
    SC.updatePersistentMapFromLTS sud ltsFileDir ltsURL snapshotsURL

    putStrLn "Hackage update..."
    HC.updateTotalArchive updateCommand unzipCommand persistCommand

    where 
        sud = (getLTSPersistDir.sui) ui
        ltsFileDir = getLTSFilesDir (sui ui)
        ltsURL = suiLTSURL (sui ui)
        snapshotsURL = (getSnapshotURL.sui) ui

        arch = (getArchive.iuh) ui
        archURL = (iuhArchiveURL.iuh) ui
        snapURL = (iuhSnapshotURL.iuh) ui
        trFile = (getTar.iuh) ui
        ud = (getArchivePersistDir.iuh) ui

        updateCommand = HC.updateArchive snapURL archURL arch 
        unzipCommand = HC.unzipArchive arch trFile 
        persistCommand = HC.updatePersistentFromTar ud trFile
{-
queryCombinedData :: UpdateInfo -> PackageName -> IO()
queryCombinedData ui package = do
    value <- HA.queryPersistentMap hUpdateDir package
    return ()
    where
        sUpdateDir = (getLTSPersistDir.sui) ui
        hUpdateDir = (getArchivePersistDir.iuh) ui

-}