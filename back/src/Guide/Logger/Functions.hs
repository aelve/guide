{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Guide.Logger.Functions
(
  -- * Loggers in 'MonadDi'
  logDebug, logInfo, logWarning, logError, logError2,
  push, attr,

  -- * Raw loggers
  logDebugIO, logInfoIO, logWarningIO, logErrorIO,
  pushLogger, attrLogger,
)
where

import Imports

import qualified Df1
import qualified Di.Core
import qualified Di

import Guide.Logger.Types

-- NB: 'Df1' provides more severity types, but they are too finely-grained
-- for us

logDebug, logInfo, logWarning, logError, logError2 :: HasLogger m => Text -> m ()
logDebug   = Di.debug   . Df1.message
logInfo    = Di.info    . Df1.message
logWarning = Di.warning . Df1.message
logError   = Di.error   . Df1.message
logError2   = Di.error   . Df1.message

logDebugIO, logInfoIO, logWarningIO, logErrorIO :: Logger -> Text -> IO ()
logDebugIO   di = Di.Core.log di Debug   . Df1.message
logInfoIO    di = Di.Core.log di Info    . Df1.message
logWarningIO di = Di.Core.log di Warning . Df1.message
logErrorIO   di = Di.Core.log di Error   . Df1.message

-- | Push context (method name, component name, etc) to the log path:
--
-- >>> push "getCategory" $ do ...
push :: HasLogger m => Text -> m a -> m a
push key = Di.push (Di.segment key)

-- | Push an attribute to the log path. Useful for giving context:
--
-- >>> push "getCategory" $ attr "catId" catId $ do ...
attr :: (HasLogger m, Show val) => Text -> val -> m a -> m a
attr key = Di.attr (Di.key key) . Di.value . show

-- | Like 'push', but operates on a 'Logger'.
pushLogger :: Text -> Logger -> Logger
pushLogger key = Di.Core.push (Df1.Push (Di.segment key))

-- | Like 'attr', but operates on a 'Logger'.
attrLogger :: Show val => Text -> val -> Logger -> Logger
attrLogger key val = Di.Core.push (Df1.Attr (Di.key key) (Di.value (show val)))
