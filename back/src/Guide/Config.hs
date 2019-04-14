{-# LANGUAGE OverloadedStrings #-}

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

-- JSON
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty as Aeson hiding (Config)
-- Default
import Data.Default

import Guide.Utils

-- ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | Site config. Stored in @config.json@.
data Config = Config {
  -- | URL where the site is deployed. Used for generating feeds (which
  -- require absolute URLs).
  _baseUrl :: Url,

  -- | Google site verification token. Will be inserted into all pages.
  _googleToken :: Text,

  -- | Password for the admin user.
  _adminPassword :: Text,

  -- | Link to a place to discuss the site. Will be placed in the header
  _discussLink :: Maybe Url,

  -- | Link to Matomo to gather analytics about user actions. Format of the
  -- link shoud be like <http://localhost:8081/piwik.php>.
  _matomoLink :: Maybe Url,

  -- | Port for serving the main site (old backend and frontend).
  _portMain :: Int,

  -- | Port for serving the API.
  _portApi :: Int,

  -- | Port for serving EKG stats.
  _portEkg :: Int,

  -- | CORS switch on/off.
  _cors :: Bool,

  -- | EKG switch on/off.
  _ekg :: Bool,

  -- | Whether to log to @stderr@.
  _logToStderr :: Bool,

  -- | Whether to log to a file. Can be turned on together with
  -- '_logToStderr'.
  _logToFile :: Maybe FilePath,

  -- | A formatting string for log timestamps. For the description of
  -- available formatters, see 'formatTime'.
  _logTimeFormat :: String
  }
  deriving (Eq, Show)

-- | Default instance: no base URL, no Google token, empty password, no
-- discussion link.
instance Default Config where
  def = Config {
    _baseUrl       = "/",
    _googleToken   = "",
    _adminPassword = "",
    _discussLink   = Nothing,
    _matomoLink    = Nothing,
    _portMain      = 8080,
    _portApi       = 4400,
    _portEkg       = 5050,
    _cors          = False,
    _ekg           = False,
    _logToStderr   = True,
    _logToFile     = Nothing,
    _logTimeFormat = "%F %T UTC"
    }

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    _baseUrl       <- o .:? "base-url"        .!= _baseUrl def
    _googleToken   <- o .:? "google-token"    .!= _googleToken def
    _adminPassword <- o .:? "admin-password"  .!= _adminPassword def
    _discussLink   <- o .:? "discuss-link"    .!= _discussLink def
    _matomoLink    <- o .:? "matomo-link"     .!= _matomoLink def
    _portMain      <- o .:? "port-main"       .!= _portMain def
    _portApi       <- o .:? "port-api"        .!= _portApi def
    _portEkg       <- o .:? "port-ekg"        .!= _portEkg def
    _cors          <- o .:? "cors"            .!= _cors def
    _ekg           <- o .:? "ekg"             .!= _ekg def
    _logToStderr   <- o .:? "log-to-stderr"   .!= _logToStderr def
    _logToFile     <- o .:? "log-to-file"     .!= _logToFile def
    _logTimeFormat <- o .:? "log-time-format" .!= _logTimeFormat def
    return Config{..}

instance ToJSON Config where
  toJSON Config{..} = object [
    "base-url"        .= _baseUrl,
    "google-token"    .= _googleToken,
    "admin-password"  .= _adminPassword,
    "discuss-link"    .= _discussLink,
    "matomo-link"     .= _matomoLink,
    "port-main"       .= _portMain,
    "port-api"        .= _portApi,
    "port-ekg"        .= _portEkg,
    "cors"            .= _cors,
    "ekg"             .= _ekg,
    "log-to-stderr"   .= _logToStderr,
    "log-to-file"     .= _logToFile,
    "log-time-format" .= _logTimeFormat
    ]

-- | Read config from @config.json@ (creating a default config if the file
-- doesn't exist).
readConfig :: IO Config
readConfig = do
  let filename = "config.json"
  exists <- doesFileExist filename
  unless exists $ do
    putStrLn "config.json doesn't exist, creating it"
    BSL.writeFile filename (Aeson.encodePretty (def :: Config))
  contents <- toLByteString <$> BS.readFile filename
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
