{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackageUpdate
(
  SnapshotInfo(..),
  fetchStackageSnapshots,
  fetchLTS,
  fetchAllLTSFiles,
  parseSnapshotInfo,
)
where

import Data.Foldable
import Data.Traversable
import Data.Aeson
-- import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Types as A

import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.String as TMS
import qualified Text.Megaparsec.Lexer as L

import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Version as DV
import Data.List as DL
import qualified Control.Exception as X
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client(parseUrlThrow)
import System.FilePath(takeDirectory, (</>))
import System.Directory(createDirectoryIfMissing)
import Common
import HttpDownload
import FileUtils

-- The method, that raises an exception, if it was not able to parse the
-- snapshot from JSON
parseSnapshotJSONThrow :: BL.ByteString -> IO [SnapshotId]
parseSnapshotJSONThrow body =
  case A.decodeWith A.json (A.parse parser) body of
    Just snapshots -> return snapshots
    Nothing -> X.throwIO $ UAE "Could not decode stackage JSON"
  where
    parser v = do
      pairs <- HM.toList <$> parseJSON v
      pure $ map (\(snapshotGroup, snapshotName) -> SnapshotId{..}) pairs

fetchStackageSnapshots :: URL -> IO [SnapshotId]
fetchStackageSnapshots url = parseUrlThrow url >>= fetchResponseData >>= parseSnapshotJSONThrow

fetchLTS :: FilePath -> URL -> IO ()
fetchLTS file url = do
  putStrLn $ "Getting LTS " ++ url ++ " to " ++ file
  removeIfExists file
  createDirectoryIfMissing True (takeDirectory file)
  writeAll2File file url

fetchAllLTSFiles :: FilePath -> URL -> [SnapshotId] -> IO()
fetchAllLTSFiles dir url ss = do
  putStrLn $ "Getting all LTS from " ++ url ++ " to directory " ++ dir
  createDirectoryIfMissing True dir
  for_ ss $ \SnapshotId{..} ->
    fetchLTS (mkyml dir snapshotName) (mkyml url snapshotName)
  where 
    mkyml pth l = pth </> (l ++ ".yaml")
    

-- | Parse a snapshot description file (e.g.
-- <https://raw.githubusercontent.com/fpco/lts-haskell/master/lts-8.9.yaml>)
parseSnapshotInfo :: BS.ByteString -> IO SnapshotInfo
parseSnapshotInfo body = case Y.decode body of
  Just datum -> return datum
  Nothing    -> X.throwIO $ UAE "Could not decode package data yaml"


data SnapshotInfo = SnapshotInfo {
  snapshotCorePackages  :: [PackageId],
  snapshotOtherPackages :: [PackageId]
  } deriving (Eq, Show)

-- This is the data, that is extracted from the yaml file
instance FromJSON SnapshotInfo where
  parseJSON = withObject "SnapshotInfo" $ \o -> do
    core <- o .: "system-info" >>= (.: "core-packages")
    snapshotCorePackages <- for (HM.toList core) $ \(name, versionStr) -> do
      version <- parseV versionStr
      return (PackageId name version)

    packages <- o .: "packages"
    snapshotOtherPackages <- for (HM.toList packages) $ \(name, content) -> do 
      versionStr <- content .: "version"
      version <- parseV versionStr
      return (PackageId name version)

    return $ SnapshotInfo{..}

    where 
      parseV vstr = case TM.parseMaybe parseVersion vstr of
        Just version -> return version
        Nothing -> fail "Count not parse"
          
parseVersion :: TMS.Parser DV.Version
parseVersion = do 
  numbers <- TM.sepBy1 L.integer (TM.char '.')
  pure $ DV.Version (map fromIntegral numbers) []

{-
type ConstraintMap = M.Map PackageName PackageData


parseStackageLTS :: Parser StackageLTS
parseStackageLTS = do
  ltsName <- parseLTS
  eol
  manyTill anyChar (string "constraints:")
  packages <- many parsePackageLine
  pure (ltsName, packages)

parseLTS :: Parser LongSnapshotName
parseLTS = do
  manyTill anyChar (string "http://www.stackage.org/snapshot/")
  name <- some (letterChar <|> digitChar <|> char '.' <|> char '-')
  pure name

parsePackageLine :: Parser PackageData
parsePackageLine = do
  space
  name <- some (letterChar <|> digitChar <|> char '-')
  space
  version <- parseVersionVer
  many (char ',') 
  space
  pure (name, version)
-}
