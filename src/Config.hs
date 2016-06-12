{-# LANGUAGE
OverloadedStrings,
RecordWildCards,
NoImplicitPrelude
  #-}


module Config
(
  Config(..),
  readConfig,
  writeConfig,
  modifyConfig,
)
where


-- General
import BasePrelude
-- Text
import Data.Text.All (Text)
-- JSON
import Data.Aeson               as Aeson
import Data.Aeson.Encode.Pretty as Aeson hiding (Config)
-- ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
-- Files
import System.Directory
-- Default
import Data.Default

-- Local
import Utils


data Config = Config {
  _baseUrl       :: Url,
  _googleToken   :: Text,
  _adminPassword :: Text,
  _prerender     :: Bool }
  deriving (Eq, Show)

instance Default Config where
  def = Config {
    _baseUrl       = "/",
    _googleToken   = "",
    _adminPassword = "",
    _prerender     = False }

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    _baseUrl       <- o .:? "base-url"       .!= _baseUrl def
    _googleToken   <- o .:? "google-token"   .!= _googleToken def
    _adminPassword <- o .:? "admin-password" .!= _adminPassword def
    _prerender     <- o .:? "prerender"      .!= _prerender def
    return Config{..}

instance ToJSON Config where
  toJSON Config{..} = object [
    "base-url"       .= _baseUrl,
    "google-token"   .= _googleToken,
    "admin-password" .= _adminPassword,
    "prerender"      .= _prerender ]

readConfig :: IO Config
readConfig = do
  let filename = "config.json"
  exists <- doesFileExist filename
  when (not exists) $ do
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

writeConfig :: Config -> IO ()
writeConfig cfg = do
  -- Create-and-rename is safer than just rewriting the file
  let newFile = "config-new.json"
  BSL.writeFile newFile (Aeson.encodePretty cfg)
  renameFile newFile "config.json"

modifyConfig :: (Config -> IO Config) -> IO ()
modifyConfig func = writeConfig =<< func =<< readConfig
