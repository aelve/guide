{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackageUpdate(fetchStackageSnapshots, fetchLTS) where

import Data.Traversable
import Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Text as T
import qualified Control.Exception as X
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client(parseUrlThrow)
import System.FilePath(takeDirectory)
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
  writeAll2File url file

