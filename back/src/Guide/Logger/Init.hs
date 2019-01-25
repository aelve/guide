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
import Data.Time.Format()
import Data.Time.Clock.System

import Guide.Config (Config (..))
import qualified Df1
import qualified Di.Core as Di
import qualified Di.Core as DC

deriving instance Read Df1.Level

initLogger :: Config -> IO (Di.Log Df1.Level Text Df1.Message -> IO ())
initLogger Config{..} = do
  logLvlEnv <- lookupEnv "LOG_LEVEL"
  let logLvl  = fromMaybe Df1.Debug (readMaybe =<< logLvlEnv)
  pure $ \(Di.Log time lvl path msg) ->
    when (lvl >= logLvl) $ do
      let
        
        formattedMsg = logLvlMark <> ": " <> logMsg
        
        logMsg :: Text
        logMsg = toStrict $ Df1.unMessage msg

        timeMark :: Text
        timeMark = T.pack (formatTime defaultTimeLocale "%a %b %e %H:%M:%S:%q %Z %Y" (systemToUTCTime time))

        logLvlMark :: Text
        logLvlMark =
            timeMark <> " " <>
            (mconcat $ intersperse "/" $ toList path) <> " " <>
            T.pack (show lvl)

      when _logToStderr   $ sayErr formattedMsg
      whenJust _logToFile $ \fileName -> do
        T.appendFile fileName (formattedMsg <> "\n")
