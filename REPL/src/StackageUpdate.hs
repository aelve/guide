{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackageUpdate(fetchStackageSnapshots, fetchLTS, fetchAllLTSFiles) where

import Data.Traversable
import Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Text as T
import Data.List as DL
import qualified Control.Exception as X
import qualified Data.ByteString.Lazy as BL
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