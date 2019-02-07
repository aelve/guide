{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for "instance Read Df1.Level"

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


-- | Catch and form log.
initLogger :: Config -> IO (Di.Log Df1.Level Df1.Path Df1.Message -> IO ())
initLogger Config{..} = do
  logLvlEnv <- lookupEnv "LOG_LEVEL"
  let logLvl  = fromMaybe Df1.Debug (readMaybe =<< logLvlEnv)
  pure $ \(Di.Log time lvl path msg) ->
    when (lvl >= logLvl) $ do
      let

        formattedMsg = logLvlMark <> " " <> logMsg

        logMsg :: Text
        logMsg = toText $ Df1.unMessage msg

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

-- | Pretty path.
unPath :: Df1.Path -> Text
unPath (Df1.Push a)   = Df1.unSegment a
unPath (Df1.Attr k v) = mconcat [Df1.unKey k, "=", toText $ Df1.unValue v]

-- | Pretty print several pathes.
printPath :: Foldable t => t Df1.Path -> Text
printPath path = " | " <> math (toList path)
  where
    math :: [Df1.Path] -> Text
    math (a : b : xs) = unPath a <> separator a b <> math (b : xs)
    math (x:_)        = unPath x <> " | "
    math []           = ""

    -- | Examples of different paths shown by 'printPath'.
{-
| Get categoty error. A wrong category id.
@
| api | getCategory catId="wseresd" | Debug handler called
| api | getCategory catId="wseresd" | Debug dbQuery: GetCategoryMaybe "wseresd"
| api | guider level | Error | ServantErr {errHTTPCode = 404, errReasonPhrase = "Category not found", errBody = "", errHeaders = []}
@

-- Put item summary with wrong original text.
@
| api | setItemSummary itemId="og29umre" | Debug dbQuery: GetItemMaybe "og29umre"
| api | setItemSummary itemId="og29umre" | Debug dbQuery: GetItemMaybe "og29umre"
| api | guider level | Error | ServantErr {errHTTPCode = 409, errReasonPhrase = "Merge conflict occurred", errBody = "{\"merged\":\"\",\"modified\":\"string\",\"server_modified\":\"\",\"original\":\"string\"}", errHeaders = []}
@
-}

separator :: Df1.Path -> Df1.Path -> Text
separator _ (Df1.Push _) = " | "
separator (Df1.Push _) _ = " "
separator _ _            = ", "


-- * Examples of 'printPath'

-- ** Get categoty error. A wrong category id.
{- |
@
| api | getCategory catId = Uid {uidToText = "fgh"} | Debug handler called
| api | getCategory catId = Uid {uidToText = "fgh"} | Debug getCategoryOrFail: Uid {uidToText = "fgh"}
| api | getCategory catId = Uid {uidToText = "fgh"} | Debug dbQuery: GetCategoryMaybe (Uid {uidToText = "fgh"})
| api | Error ServantErr {errHTTPCode = 404, errReasonPhrase = "Category not found", errBody = "", errHeaders = []}
@
-}

-- ** Put item summary with wrong original text.
{- |
@
| api | setItemSummary itemId = Uid {uidToText = "og29umre"} | Debug handler called
| api | setItemSummary itemId = Uid {uidToText = "og29umre"} | Debug getItemOrFail: Uid {uidToText = "og29umre"}
| api | setItemSummary itemId = Uid {uidToText = "og29umre"} | Debug dbQuery: GetItemMaybe (Uid {uidToText = "og29umre"})
| api | setItemSummary itemId = Uid {uidToText = "og29umre"} | Debug checkConflict
| api | Error ServantErr {errHTTPCode = 409, errReasonPhrase = "Merge conflict occurred", errBody = "{\"merged\":\"\",\"modified\":\"string\",\"server_modified\":\"\",\"original\":\"d\"}", errHeaders = []}
@
-}
