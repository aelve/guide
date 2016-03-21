{-# LANGUAGE
OverloadedStrings,
RecordWildCards,
NoImplicitPrelude
  #-}


module Config
(
  Config(..),
  readConfig,
  modifyConfig,
)
where


-- General
import BasePrelude
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
  _trackingEnabled :: Bool,
  _baseUrl         :: Url }
  deriving (Eq, Show)

instance Default Config where
  def = Config {
    _trackingEnabled = False,
    _baseUrl         = "/" }

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    _trackingEnabled <- o .:? "tracking-enabled" .!= _trackingEnabled def
    _baseUrl         <- o .:? "base-url"         .!= _baseUrl def
    return Config{..}

instance ToJSON Config where
  toJSON Config{..} = object [
    "tracking-enabled" .= _trackingEnabled,
    "base-url"         .= _baseUrl ]

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
    Right cfg -> return cfg

modifyConfig :: (Config -> IO Config) -> IO ()
modifyConfig func = do
  file <- readConfig
  -- Create-and-rename is safer than just rewriting the file
  let newFile = "config-new.json"
  BSL.writeFile newFile . Aeson.encodePretty =<< func file
  renameFile newFile "config.json"
