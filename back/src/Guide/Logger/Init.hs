{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- for "instance Read Df1.Level"

module Guide.Logger.Init
(
  initLogger,
  DC.new,
)
where

import Imports
import Say (sayErr)
import Control.Monad.Extra
import qualified Data.Text.IO as T
import Data.Time.Format ()
import Data.Time.Clock.System

import Guide.Config (Config (..))
import qualified Df1
import qualified Di.Core as Di
import qualified Di.Core as DC

deriving instance Read Df1.Level


-- | Catch exceptions and make a log.
--
-- Example of resulting log:
--
-- > api | getCategory catId="og29umre" | Debug Handler called
-- > api | getCategory catId="og29umre" | Debug dbQuery: GetCategoryMaybe "og29umre"
-- > api | Error ServantErr {errHTTPCode = 404, errReasonPhrase = "Category not found", errBody = "", errHeaders = []}
initLogger :: Config -> IO (Di.Log Df1.Level Df1.Path Df1.Message -> IO ())
initLogger Config{..} = do
  logLvlEnv <- lookupEnv "LOG_LEVEL"
  let logLvl = fromMaybe Df1.Debug (readMaybe =<< logLvlEnv)
  pure $ \(Di.Log time lvl path msg) ->
    when (lvl >= logLvl) $ do
      let
        formattedMsg :: Text
        formattedMsg =
          format "{} | {} | {} {}" timeMark (showPath path) (show lvl) logMsg

        logMsg :: Text
        logMsg = toText $ Df1.unMessage msg

        timeMark :: Text
        timeMark = toText $
          formatTime defaultTimeLocale _logTimeFormat (systemToUTCTime time)

      when _logToStderr   $ sayErr formattedMsg
      whenJust _logToFile $ \fileName -> do
        T.appendFile fileName (formattedMsg <> "\n")

-- | Pretty-print a log path.
--
-- >>> showPath [Push "api", Push "createItem", Attr "catId" "wseresd", Attr "name" "Foo"]
-- "api | getCategory catId=\"wseresd\" name=\"Foo\""
showPath :: Foldable t => t Df1.Path -> Text
showPath path = go (toList path)
  where
    go :: [Df1.Path] -> Text
    go (a : b : xs) = showPiece a <> separator a b <> go (b : xs)
    go [a]          = showPiece a
    go []           = ""

    showPiece :: Df1.Path -> Text
    showPiece = \case
      Df1.Push a -> Df1.unSegment a
      Df1.Attr k v -> mconcat [Df1.unKey k, "=", toText (Df1.unValue v)]

    separator :: Df1.Path -> Df1.Path -> Text
    separator _ (Df1.Push _) = " | "
    separator _ _ = " "
