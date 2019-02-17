-- | A small logging framework for Guide, implemented as a wrapper over the
-- "di" package.
--
-- Used mostly in "Guide.Main" and "Guide.Api.Methods".
--
-- Some internals are exposed in modules under @Guider.Logger.*@, but you
-- shouldn't need them. If you do, consider exporting them from this module.
module Guide.Logger
(
  -- * Creating loggers
  withLogger,

  -- * Logging with 'LoggerT'
  HasLogger,
  LoggerT, runLoggerT,
  logDebug, logInfo, logWarning, logError,
  -- ** Building log paths
  push, attr,

  -- * Logging without a monad transformer
  Logger,
  logDebugIO, logInfoIO, logWarningIO, logErrorIO,
  -- ** Building log paths
  pushLogger, attrLogger,
)
where

import Guide.Logger.Types
import Guide.Logger.Functions
import Guide.Logger.Run
