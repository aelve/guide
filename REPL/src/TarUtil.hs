module TarUtil (getEntries,
                loadTarIndex,
                buildHackageMap,
                buildDifferenceMap
                ) where

import qualified Codec.Archive.Tar.Index as TI
import qualified Codec.Archive.Tar as Tar
import qualified Data.List.Split as SPLT
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Version as DV

import qualified Data.Map.Strict as Map
import System.FilePath.Posix(hasTrailingPathSeparator)
import Control.Monad(guard)

import qualified Text.ParserCombinators.ReadP as RP

-- The record for each of the package from hackage
-- TODO - add another information about the packages
data HackagePackage = HP {
  name :: String,
  version :: DV.Version
} deriving (Eq, Show)

-- The status of the package between two updates
data HackageUpdate = Added | Removed | Updated deriving (Eq, Show)

-- The map of all the hackage packages with name as the key and HackagePackage
-- as the value
type HackageMap = Map.Map String HackagePackage

-- The map, that shows, which packages have change since the last update
type HackageUpdateMap = Map.Map String (HackageUpdate, HackagePackage)

-- Parses the file path of the cabal file to get version and package name
parseCabalFilePath :: RP.ReadP HackagePackage
parseCabalFilePath = do
  package <- RP.munch1 DC.isLetter
  RP.char '/'
  version <- DV.parseVersion
  RP.char '/'
  name <- RP.munch1 DC.isLetter
  guard (name == package)
  suff <- RP.string ".cabal"
  RP.eof
  pure $ HP { name = package, version = version}

-- Update map of the packages with the hackage package
-- Update when, the version of package is newer than version of package in the
-- map
updateMap :: HackagePackage -> HackageMap -> HackageMap
updateMap hp map = case Map.lookup (name hp) map of
  Just oldHp -> if (version hp) > (version oldHp) then updatedMap
                                                  else map
  Nothing -> updatedMap
  where updatedMap = Map.insert (name hp) hp map

getEntries :: TI.TarIndex -> [HackagePackage]
getEntries index =  map fst $ map head $ filter (not.null) $ map (goodParse.parse.getPath) entries
  where entries = TI.toList index
        getPath = fst
        parse = RP.readP_to_S parseCabalFilePath
        goodParse = filter (null.snd)
        
loadTarIndex :: FilePath -> IO (Either Tar.FormatError TI.TarIndex)
loadTarIndex file = do
  content <- BL.readFile file
  return $ TI.build $ Tar.read content


-- convert tarindex to list, then apply parser combinator, throw out all
-- empty parsingresults and then take the first successfull parsing result 
buildHackageMap :: TI.TarIndex -> HackageMap
buildHackageMap index = foldr updateMap Map.empty (getEntries index)

buildDifferenceMap :: HackageMap -> HackageMap -> HackageUpdateMap
buildDifferenceMap oldMap newMap = foldr Map.union Map.empty [deletedMap, addedMap, updatedMap]
  where
    deletedMap = Map.map ((,) Removed) $ Map.difference oldMap newMap
    addedMap = Map.map ((,) Added) $ Map.difference newMap oldMap
    updatedMap' = Map.intersection newMap oldMap
    updatedMap = Map.map ((,) Updated) $ Map.differenceWith diff updatedMap' oldMap
    diff newpack oldpack = if (newpack /= oldpack) then Just newpack else Nothing
