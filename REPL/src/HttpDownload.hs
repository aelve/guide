{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpDownload(
                    fetchResponseData,
                    fetchRangeData,
                    Range,
                    cropRanges) where
import Data.Int(Int64)

import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client(Request(..), parseUrlThrow, newManager, responseBody, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header

import Common

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
fetchResponseData req = responseBody <$> (newManager tlsManagerSettings >>= httpLbs req)

-- Returns the bytes from the range request
fetchRangeData :: URL -> Range -> IO BL.ByteString
fetchRangeData url range = createRangeRequest url range >>= fetchResponseData

