module StackageCommands(
                        showSnapshots,
                        updateLTSFile, 
                        updateAllLTSFiles) where

import Common              
import StackageUpdate

showSnapshots :: URL -> IO()
showSnapshots url = do
    SSS snapshots <- fetchStackageSnapshots url
    putStrLn $ "Showing snapshots from " ++ url
    mapM_ (putStrLn.(\s -> "\tSnapshot: " ++ s).show) snapshots

-- updates the lts file from github
updateLTSFile :: FilePath -> URL -> IO ()
updateLTSFile = fetchLTS 

-- updates all of the lts files from the snapshot file at stackage

updateAllLTSFiles :: FilePath -> URL -> URL -> IO ()
updateAllLTSFiles ltsDir ltsURL snapshotsURL = do
    snapshots <- fetchStackageSnapshots snapshotsURL
    fetchAllLTSFiles ltsDir ltsURL snapshots