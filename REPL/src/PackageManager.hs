-- This is the only module, needed to use the functionality of persistent storage and updating of the packages
-- It reexports queryCombinedData, function, that looks up package in both 
-- hackage and stackage persistent storages and launchPackageUpdater - that
-- can be launched with the config and delay and will update the hackage and stackage archive
-- after every specified time period (time period is specified in minutes)

module PackageManager(  UpdateInfo(..), 
                        CombinedPackage (..),
                        queryCombinedData,
                        launchPackageUpdater) where

import REPL
import AllCommands


launchPackageUpdater :: UpdateInfo -> Int -> IO ()
launchPackageUpdater ui = processDelayCycle ui updateAllCommand