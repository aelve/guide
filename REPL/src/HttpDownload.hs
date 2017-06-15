{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpDownload(
                    fetchResponseData,
                    fetchRangeData,
                    Range,
                    calculateContentSize,
                    cropRanges,
                    write2File,
                    writeAll2File) where
import Data.Int(Int64)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (readInteger)
import Network.HTTP.Client(Request(..), parseUrlThrow, newManager, responseBody, httpLbs, responseHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import qualified Control.Exception as X

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

makeHeadRequest :: Request -> Request
makeHeadRequest r = r {
  method = "HEAD"
}

getContentLength :: ResponseHeaders -> Maybe BS.ByteString
getContentLength = lookup hContentLength

-- Try to calculate the size of the resource based on the head request
calculateContentSize :: URL -> IO Int64
calculateContentSize url = do 
  request <- makeHeadRequest <$> parseUrlThrow url 
  response <- newManager tlsManagerSettings >>= httpLbs request
  print response
  case (getContentLength.responseHeaders) response >>= readInteger of
    Just (size, _) -> return $ fromIntegral size
    _ -> X.throwIO $ UAE $ "Content size not parsed from " ++ url


write2File :: FilePath -> URL -> Range -> IO() 
write2File archive url range = do
  putStrLn $ "\tGetting range " ++ show range ++ " from " ++ url
  body <- fetchRangeData url range
  putStrLn $ "\tGot range " ++ show (BL.take 50 body)
  BL.appendFile archive body
  putStrLn "Append ok"


writeAll2File :: FilePath -> URL -> IO()
writeAll2File archive url = do
  putStrLn $ "\tGetting body from " ++ url
  body <- parseUrlThrow url >>= fetchResponseData
  BL.appendFile archive body
  putStrLn "Write ok"

