{-# LANGUAGE OverloadedStrings #-}

{- |
Server config. For instance, the admin password is stored here, as well as
the base url (for correct link generation in feeds).
-}
module Guide.Config
(
  Config(..),
  readConfig,
  writeConfig,
  modifyConfig,
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
  _baseUrl       :: Url,          -- ^ URL where the site is deployed. Used
                                  --    for generating feeds (which require
                                  --    absolute URLs)
  _googleToken   :: Text,         -- ^ Google site verification token. Will
                                  --    be inserted into all pages
  _adminPassword :: Text,         -- ^ Password for the admin user
  _prerender     :: Bool,         -- ^ Whether to prerender all pages when
                                  --    the app is started
  _discussLink   :: Maybe Url     -- ^ Link to a place to discuss the site.
                                  --    Will be placed in the header
  }
  deriving (Eq, Show)

-- | Default instance: no base URL, no Google token, empty password, no
-- prerendering, no discussion link.
instance Default Config where
  def = Config {
    _baseUrl       = "/",
    _googleToken   = "",
    _adminPassword = "",
    _prerender     = False,
    _discussLink   = Nothing }

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    _baseUrl       <- o .:? "base-url"       .!= _baseUrl def
    _googleToken   <- o .:? "google-token"   .!= _googleToken def
    _adminPassword <- o .:? "admin-password" .!= _adminPassword def
    _prerender     <- o .:? "prerender"      .!= _prerender def
    _discussLink   <- o .:? "discuss-link"   .!= _discussLink def
    return Config{..}

instance ToJSON Config where
  toJSON Config{..} = object [
    "base-url"       .= _baseUrl,
    "google-token"   .= _googleToken,
    "admin-password" .= _adminPassword,
    "prerender"      .= _prerender,
    "discuss-link"   .= _discussLink ]

-- | Read config from @config.json@ (creating a default config if the file
-- doesn't exist).
readConfig :: IO Config
readConfig = do
  let filename = "config.json"
  exists <- doesFileExist filename
  unless exists $ do
    putStrLn "config.json doesn't exist, creating it"
    BSL.writeFile filename (Aeson.encodePretty (def :: Config))
  contents <- BSL.fromStrict <$> BS.readFile filename
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
