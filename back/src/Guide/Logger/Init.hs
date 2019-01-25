{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Guide.Logger.Init (
  initLogger,
  DC.new,
  ) where

import Imports
import Say (sayErr)
import Control.Monad.Extra
import qualified Data.Text.IO   as T
import qualified Data.Text      as T

import Guide.Config (Config (..))
import qualified Df1
import qualified Di.Core as Di
import qualified Di.Core as DC

deriving instance Read Df1.Level

initLogger :: Config -> IO (Di.Log Df1.Level Text Df1.Message -> IO ())
initLogger Config{..} = do
  logLvlEnv <- lookupEnv "LOG_LEVEL"
  let logLvl  = fromMaybe Df1.Debug (readMaybe =<< logLvlEnv)
  pure $ \(Di.Log _ lvl _ msg) ->
    when (lvl >= logLvl) $ do
      let
        
        formattedMsg = logLvlMark <> logMsg
        
        logMsg :: Text
        logMsg = toStrict $ Df1.unMessage msg

        logLvlMark :: Text
        logLvlMark = T.pack (show lvl) <> " "

      when _logToStderr   $ sayErr formattedMsg
      whenJust _logToFile $ \fileName -> do
        T.appendFile fileName (formattedMsg <> "\n")
