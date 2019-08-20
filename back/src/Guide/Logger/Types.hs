{-# OPTIONS_GHC -fno-warn-orphans #-} -- for "instance Read Df1.Level"

-- | The logger monad and associated types.
module Guide.Logger.Types
(
  -- * Main types
  HasLogger,
  LoggerT, runLoggerT,
  Logger,

  -- * Internals
  -- ** Log pieces
  LogLine,
  Df1.Level(..),
  Df1.Path(..),
  Df1.Message,
)
where

-- Shared imports
import Imports

import Di.Core
import Di.Monad

import qualified Df1

-- | Monads where logging is possible.
--
-- This is a type synonym, but you can still write @deriving HasLogger@.
type HasLogger = MonadDi Df1.Level Df1.Path Df1.Message

-- | Monad transformer that allows logging with functions in "Guide.Logger".
type LoggerT = DiT Df1.Level Df1.Path Df1.Message

-- | Execute a 'LoggerT' given an existing logger.
runLoggerT :: MonadIO m => Logger -> LoggerT m a -> m a
runLoggerT = runDiT

-- | A structure that allows logging without a 'LoggerT'.
type Logger = Di Df1.Level Df1.Path Df1.Message

-- | A single log message together with all attributes.
type LogLine = Log Df1.Level Df1.Path Df1.Message

----------------------------------------------------------------------------
-- Orphans
----------------------------------------------------------------------------

deriving instance Read Df1.Level
