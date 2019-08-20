{-# LANGUAGE FlexibleContexts    #-}

module Guide.Logger.Run
(
  withLogger,
)
where

-- Shared imports
import Imports

import Say (sayErr, hSay)
import Control.Monad.Extra
import Data.Time.Clock.System
import System.IO

import qualified Data.Text as T
import qualified Df1
import qualified Di.Core

import Guide.Config
import Guide.Logger.Types
import Guide.Logger.Functions

-- | Create a 'Logger' for the given 'Config' and run an action with that
-- logger. Depending on the 'Config', we might log to @stderr@, to a file,
-- or both.
--
-- Log level is customizable by setting the @LOG_LEVEL@ environment
-- variable.
--
-- /Exception handling:/
--
-- Uncaught exceptions are caught and logged. Since Servant exceptions don't
-- escape Servant, they are logged where they are thrown (see @instance
-- MonadError Guider@). Uncaught Servant exceptions are caught and logged in
-- Warp.
withLogger :: Config -> (Logger -> IO ()) -> IO ()
withLogger Config{..} act = do
  logLvlEnv <- lookupEnv "LOG_LEVEL"
  let logLvl = fromMaybe Debug (readMaybe =<< logLvlEnv)
  mbWithFile logToFile AppendMode $ \logFileHandle -> do
    let logHandler logLine@(Di.Core.Log _ lvl _ _) =
          when (lvl >= logLvl) $ do
            let formattedLogLine = showLogLine logTimeFormat logLine
            when logToStderr $ sayErr formattedLogLine
            whenJust logFileHandle $ \h -> hSay h formattedLogLine
    Di.Core.new logHandler $ \logger ->
      act logger `catch` \(e :: SomeException) ->
        logErrorIO logger ("uncaught exception: "+||e||+"")

-- | Pretty-print a log line.
showLogLine
  :: String        -- ^ Time format
  -> LogLine
  -> Text
showLogLine timeFormat (Di.Core.Log time lvl path msg) =
  format "[{}] {}: {} | {}" time' (show lvl) path' msg'
  where
    time' = formatTime defaultTimeLocale timeFormat (systemToUTCTime time)
    path' = case showPath path of
      "" -> "<root>"
      s -> s
    msg'  = T.replace "\n" ";" (toText (Df1.unMessage msg))

-- | Pretty-print a log path.
--
-- >>> showPath [Push "api", Push "createItem", Attr "catId" "wseresd", Attr "name" "Foo"]
-- "api > getCategory catId=\"wseresd\" name=\"Foo\""
showPath :: Foldable t => t Path -> Text
showPath path = go (toList path)
  where
    go :: [Path] -> Text
    go (a : b : xs) = showPiece a <> separator a b <> go (b : xs)
    go [a]          = showPiece a
    go []           = ""

    showPiece :: Path -> Text
    showPiece = \case
      Df1.Push a -> toText (Df1.unSegment a)
      Df1.Attr k v -> toText $ mconcat [Df1.unKey k, "=", Df1.unValue v]

    separator :: Path -> Path -> Text
    separator _ (Push _) = " > "
    separator _ _ = " "

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Like 'withFile', but on @Maybe FilePath@.
mbWithFile :: Maybe FilePath -> IOMode -> (Maybe Handle -> IO r) -> IO r
mbWithFile Nothing _ act = act Nothing
mbWithFile (Just fp) mode act = withFile fp mode (act . Just)
