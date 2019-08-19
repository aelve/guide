-- | Methods for working with archive.org. Right now the admin interface
-- provides a list of broken links together with links to their archived
-- copies; in the future we might add automatic archival and automatic link
-- replacement.
module Guide.Archival
(
  ArchivalStatus(..),
  getArchivalStatus,
)
where

import Imports

-- network
import Network.HTTP.Client

import Guide.Utils

-- JSON
import qualified Data.Aeson as Aeson

-- | Get status of a link on archive.org.
--
-- 'Left' means that an error happened when connecting to archive.org, or
-- that its response couldn't be parsed.
getArchivalStatus :: Manager -> Url -> IO (Either String ArchivalStatus)
getArchivalStatus manager lnk =
  handle (pure . Left . show @HttpException) $ do
    req <- setQueryString [("url", Just (toUtf8ByteString lnk))] <$>
               parseRequest waybackUrl
    fromJsonWith responseParser . responseBody <$!> httpLbs req manager
  where
    waybackUrl = "http://archive.org/wayback/available"
    responseParser = Aeson.withObject "archive.org response" $
        (Aeson..: "archived_snapshots") >=> (Aeson..: "closest")

data ArchivalStatus = ArchivalStatus {
  asAvailable :: Bool,     -- ^ Whether the link is available
  asUrl       :: Url,      -- ^ Link to archived page
  asTimestamp :: UTCTime,  -- ^ When the page was archived
  asStatus    :: Text }    -- ^ HTTP status ("200", "404", etc)
  deriving (Eq, Show)

-- For an example, look at archived_snapshots.closest in
-- <http://archive.org/wayback/available?url=example.com>:
--
-- { "status": "200"
-- , "available": true
-- , "url": "http://web.archive.org/web/20170819042701/http://example.com"
-- , "timestamp": "20170819042701" }
instance Aeson.FromJSON ArchivalStatus where
  parseJSON = Aeson.withObject "ArchivalStatus" $ \o -> do
    asAvailable <- o Aeson..: "available"
    asUrl       <- o Aeson..: "url"
    asStatus    <- o Aeson..: "status"
    asTimestamp <- o Aeson..: "timestamp" >>=
                   parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S"
    pure ArchivalStatus{..}
