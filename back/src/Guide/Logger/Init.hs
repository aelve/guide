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

initLogger :: Config -> IO (Di.Log Df1.Level Df1.Path Df1.Message -> IO ())
initLogger Config{..} = do
  logLvlEnv <- lookupEnv "LOG_LEVEL"
  let logLvl  = fromMaybe Df1.Debug (readMaybe =<< logLvlEnv)
  pure $ \(Di.Log time lvl path msg) ->
    when (lvl >= logLvl) $ do
      let
        
        formattedMsg = logLvlMark <> " " <> logMsg
        
        logMsg :: Text
        logMsg = toStrict $ Df1.unMessage msg

        timeMark :: Text
        timeMark = T.pack (formatTime defaultTimeLocale _logTimeFormat (systemToUTCTime time))

        logLvlMark :: Text
        logLvlMark =
            timeMark <>
            (printPath $ toList path) <>
            T.pack (show lvl)

      when _logToStderr   $ sayErr formattedMsg
      whenJust _logToFile $ \fileName -> do
        T.appendFile fileName (formattedMsg <> "\n")

unPath :: Df1.Path -> Text
unPath (Df1.Push a)   = Df1.unSegment a
unPath (Df1.Attr k v) = mconcat [Df1.unKey k, " = ", toStrict $ Df1.unValue v]

printPath :: Foldable t => t Df1.Path -> Text
printPath path = " | " <> math (toList path)
  where
    math :: [Df1.Path] -> Text
    math (a : b : xs) = unPath a <> separator a b <> math (b : xs)
    math (x:_)        = unPath x <> " | "
    math []           = ""

separator :: Df1.Path -> Df1.Path -> Text
separator _ (Df1.Push _) = " | "
separator (Df1.Push _) _ = " "
separator _ _            = ", "