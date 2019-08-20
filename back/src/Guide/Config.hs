-- | Server config. For instance, the admin password is stored here, as well
-- as the base url (for correct link generation in feeds).
module Guide.Config
(
  Config(..),
  readConfig,
  writeConfig,
  modifyConfig,
  def,
)
where


import Imports hiding ((.=))

import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty as Aeson hiding (Config)
import Data.Default
import Say (sayErr)

import Guide.Utils

-- ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | Site config. Stored in @config.json@.
data Config = Config {
  -- | URL where the site is deployed. Used for generating feeds (which
  -- require absolute URLs).
  baseUrl :: Url,

  -- | Google site verification token. Will be inserted into all pages.
  googleToken :: Text,

  -- | Password for the admin user.
  adminPassword :: Text,

  -- | Link to a place to discuss the site. Will be placed in the header
  discussLink :: Maybe Url,

  -- | Link to Matomo to gather analytics about user actions. Format of the
  -- link shoud be like <http://localhost:8081/piwik.php>.
  matomoLink :: Maybe Url,

  -- | Port for serving the main site (old backend and frontend).
  portMain :: Int,

  -- | Port for serving the API.
  portApi :: Int,

  -- | CORS switch on/off.
  cors :: Bool,

  -- | Whether to log to @stderr@.
  logToStderr :: Bool,

  -- | Whether to log to a file. Can be turned on together with
  -- 'logToStderr'.
  logToFile :: Maybe FilePath,

  -- | A formatting string for log timestamps. For the description of
  -- available formatters, see 'formatTime'.
  logTimeFormat :: String
  }
  deriving (Eq, Show)

$(pure [])

-- | Default instance: no base URL, no Google token, empty password, no
-- discussion link.
instance Default Config where
  def = Config {
    baseUrl       = "/",
    googleToken   = "",
    adminPassword = "",
    discussLink   = Nothing,
    matomoLink    = Nothing,
    portMain      = 8080,
    portApi       = 4400,
    cors          = False,
    logToStderr   = True,
    logToFile     = Nothing,
    logTimeFormat = "%F %T UTC"
    }

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    baseUrl       <- o .:? "base-url"        .!= baseUrl def
    googleToken   <- o .:? "google-token"    .!= googleToken def
    adminPassword <- o .:? "admin-password"  .!= adminPassword def
    discussLink   <- o .:? "discuss-link"    .!= discussLink def
    matomoLink    <- o .:? "matomo-link"     .!= matomoLink def
    portMain      <- o .:? "port-main"       .!= portMain def
    portApi       <- o .:? "port-api"        .!= portApi def
    cors          <- o .:? "cors"            .!= cors def
    logToStderr   <- o .:? "log-to-stderr"   .!= logToStderr def
    logToFile     <- o .:? "log-to-file"     .!= logToFile def
    logTimeFormat <- o .:? "log-time-format" .!= logTimeFormat def
    return Config{..}

instance ToJSON Config where
  toJSON $(fields 'Config) = object [
    "base-url"        .= baseUrl,
    "google-token"    .= googleToken,
    "admin-password"  .= adminPassword,
    "discuss-link"    .= discussLink,
    "matomo-link"     .= matomoLink,
    "port-main"       .= portMain,
    "port-api"        .= portApi,
    "cors"            .= cors,
    "log-to-stderr"   .= logToStderr,
    "log-to-file"     .= logToFile,
    "log-time-format" .= logTimeFormat
    ]

-- | Read config from @config.json@ (creating a default config if the file
-- doesn't exist).
readConfig :: IO Config
readConfig = do
  let filename = "config.json"
  exists <- doesFileExist filename
  unless exists $ do
    sayErr "config.json doesn't exist, creating it"
    BSL.writeFile filename (Aeson.encodePretty (def :: Config))
  contents <- toLazyByteString <$> BS.readFile filename
  case Aeson.eitherDecode' contents of
    Left err  -> error ("error when reading config: " ++ err)
    Right cfg -> do
      -- If after an update there are new fields in the config, we should add
      -- them to the file – which can be done by writing the config to the
      -- file after we've read it.
      writeConfig cfg
      return cfg

-- | Write a config to @config.json@.
writeConfig :: Config -> IO ()
writeConfig cfg = do
  -- Create-and-rename is safer than just rewriting the file
  let newFile = "config-new.json"
  BSL.writeFile newFile (Aeson.encodePretty cfg)
  renameFile newFile "config.json"

-- | Apply a function to the config.
modifyConfig :: (Config -> IO Config) -> IO ()
modifyConfig func = writeConfig =<< func =<< readConfig
