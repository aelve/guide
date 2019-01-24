{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Guide.Api.Logger where

import Imports
import Say (sayErr)

import Guide.Config (Config (..))
import qualified Df1
import qualified Di.Core as Di

deriving instance Read Df1.Level

initLogger :: Config -> IO (Di.Log Df1.Level Text Text -> IO ())
initLogger _ = do
    mLevel <- lookupEnv "LOG_LEVEL"
    let logLvl = fromMaybe Df1.Debug (readMaybe =<< mLevel)
    pure $ \(Di.Log _ lvl _ msg) ->
        when (lvl >= logLvl) $ sayErr msg
