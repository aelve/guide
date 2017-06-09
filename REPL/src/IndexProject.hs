module IndexProject(HackageUpdateInfo(..), 
                    HackageName(..),
                    processCycle,
                    updateHackageMap,
                    queryHackageMap
                    ) where

import REPL (HackageUpdateInfo(..), processCycle, updateArchive, updateMapFromTar, queryHackageMap)
import HackageArchive (HackageName (..), HackagePackage(..))

updateHackageMap :: HackageUpdateInfo -> IO ()
updateHackageMap iuh = updateArchive iuh >> updateMapFromTar iuh
