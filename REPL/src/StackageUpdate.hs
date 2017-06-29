{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackageUpdate(fetchStackageSnapshots, 
                      fetchLTS, 
                      fetchAllLTSFiles,
                      parseYamlFileThrow) where

import Data.Traversable
import Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP

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

instance FromJSON StackageSnapshots where
  parseJSON = withObject "snapshots" $ \o ->
    -- I have 'o', which is a HashMap. 
    SSS <$> (for (HM.toList o) $ \(shortName, longNameVal) -> do 
        longName <- parseJSON longNameVal
        return (T.unpack shortName, longName))

-- The method, that raises an exception, if it was not able to parse the
-- snapshot from JSON
parseSnapshotJSONThrow :: BL.ByteString -> IO StackageSnapshots
parseSnapshotJSONThrow body = case A.decode body of 
  (Just snapshots) -> return snapshots
  Nothing -> X.throwIO $ UAE "Could not decode stackage JSON"

fetchStackageSnapshots :: URL -> IO StackageSnapshots
fetchStackageSnapshots url = parseUrlThrow url >>= fetchResponseData >>= parseSnapshotJSONThrow

fetchLTS :: FilePath -> URL -> IO ()
fetchLTS file url = do
  putStrLn $ "Getting LTS " ++ url ++ " to " ++ file
  removeIfExists file
  createDirectoryIfMissing True (takeDirectory file)
  writeAll2File file url

fetchAllLTSFiles :: FilePath -> URL -> StackageSnapshots -> IO()
fetchAllLTSFiles dir url (SSS snapshots) = do
  putStrLn $ "Getting all LTS from " ++ url ++ " to directory " ++ dir
  createDirectoryIfMissing True dir
  
  mapM_ (\(_, l) -> fetchLTS (mkyml dir l) (mkyml url l)) $ filter (\(_, l) -> DL.isPrefixOf "lts" l) snapshots
  where 
    mkyml pth l = pth </> (l ++ ".yaml")

parseYamlFileThrow :: BS.ByteString -> IO PackageDatum
parseYamlFileThrow body = case Y.decode body of
  (Just datum) -> return datum
  Nothing -> X.throwIO $ UAE "Could not decode package data yaml"



-- This is the data, that is extracted from the yaml file
instance FromJSON PackageDatum where
  parseJSON = withObject "bigfatyaml" $ \o -> do
    systemO <- o .: "system-info"
    coreO <- systemO .: "core-packages"

    pkgCore <- for (HM.toList coreO) $ \(name :: String, versionStr :: String) -> do
      version <- parseV versionStr
      return (name, version)    

    packagesO <- o .: "packages"

    pkgAll <- for (HM.toList packagesO) $ \(name :: String, content) -> do 
      (versionStr :: String) <- content .: "version"
      version <- parseV versionStr
      return (name, version)

    return $ PD (pkgCore ++ pkgAll)

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