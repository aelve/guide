{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpDownload(HackageSnapshotData,
                    UpdateArchiveException(..),
                    fetchSnapshot,
                    fetchRangeData,
                    Range,
                    cropRanges
                    ) where
import Data.Aeson.Types
import Data.Int(Int64)
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client(Request(..), parseUrlThrow, newManager, responseBody, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import qualified Control.Exception as X

import Common

type HackageSnapshotData = SnapshotData

-- This is the data that is extracted from the path to cabal file
-- Like, when program parses "safeio/0.0.2.0/safeio.cabal"
-- It gets the version 0.0.2.0 and safeio package name. Also checks, xxx and yyy match in 
-- "xxx/version/yyy.cabal
instance FromJSON SnapshotData where
  parseJSON = withObject "snapshot" $ \o -> do
    signedO <- o .: "signed"
    metaO <- signedO .: "meta"
    tarO <- metaO .: "<repo>/01-index.tar.gz"
    hashesO <- tarO .: "hashes"
    md5str <- hashesO .: "md5"
    len <- tarO .: "length"
    return (SnapshotData md5str len)

-- The range, from which to download 
type Range = (Int64, Int64)

-- Chops the range into the list of ranges, for adequate downloading
cropRanges :: Int64 -> Range -> [Range]
cropRanges maxRange (from, to) 
 | to - from + 1 <= maxRange = [(from, to)]
 | otherwise = (from, from + maxRange - 1) : cropRanges maxRange (from + maxRange, to)

-- Creates the request by parsing url and then modifies it to make range request
createRangeRequest :: URL -> Range -> IO Request
createRangeRequest url range = makeRangeRequest range <$> parseUrlThrow url

-- Writes the range to the simple http request
makeRangeRequest :: Range -> Request -> Request
makeRangeRequest (from, to) = makeRange
  where 
    br = ByteRangeFromTo (fromIntegral from) (fromIntegral to)
    makeRange r = r {
      requestHeaders = (hRange, renderByteRanges [br]) : requestHeaders r
    }

-- Returns the data from response, returned to the request
fetchResponseData :: Request -> IO BL.ByteString
fetchResponseData req = newManager tlsManagerSettings >>= httpLbs req >>= return.responseBody

-- Returns the bytes from the range request
fetchRangeData :: URL -> Range -> IO BL.ByteString
fetchRangeData url range = createRangeRequest url range >>= fetchResponseData

-- The method, that raises an exception, if it was not able to parse the
-- snapshot from JSON
parseSnapshotJSONThrow :: BL.ByteString -> IO HackageSnapshotData
parseSnapshotJSONThrow body = case A.decode body of 
  (Just snapshot) -> return snapshot
  Nothing -> X.throwIO $ UAE "Could not decode JSON"

-- Returns the snapshot of archive from the hackage
fetchSnapshot :: URL -> IO HackageSnapshotData
fetchSnapshot url = parseUrlThrow url >>= fetchResponseData >>= parseSnapshotJSONThrow


