{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | 'Guider' monad with 'Config' to replace servant's 'Handler'.
module Guide.Api.Guider
(
  Guider (..),
  Context (..),
  guiderToHandler,
)
where

import Imports

import Servant (Handler (..), ServantErr (..))

import Guide.Api.Utils (RequestDetails)
import Guide.Config (Config)
import Guide.Logger
import Guide.State (DB)

import qualified Control.Monad.Catch as Exc


-- | A type for Guide handlers. Provides:
--
-- * Logging via 'LoggerT'
-- * Access to 'Context'
--
-- Note that it's not simply a wrapper over 'Handler' -- we throws
-- 'ServantErr's as synchronous exceptions and then catch them in
-- 'guiderToHandler'. It makes our lives easier because now there is exactly
-- one way to throw errors, instead of two.
newtype Guider a = Guider
  -- NB: we don't want to move 'Logger' to the 'Context' because 'LoggerT'
  -- also contains some STM weirdness (even though it's still a reader
  -- monad).
  { runGuider :: ReaderT Context (LoggerT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context, HasLogger, Exc.MonadThrow)

-- | Context available to each request.
data Context = Context
  { cConfig  :: Config
  , cDB      :: DB
  , cDetails :: RequestDetails
  }

instance MonadError ServantErr Guider where
  -- | Log an error and rethrow it.
  throwError :: ServantErr -> Guider a
  throwError err = do
    let code = errHTTPCode err
        reason = errReasonPhrase err
    if | code >= 500 -> logError ("error: "+||err||+"")
       | otherwise -> logDebug ("response code "+|code|+": "+|reason|+"")
    Exc.throwM err

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f = Guider $ ReaderT $ \context ->
    runReaderT m context `Exc.catch` (\err -> runReaderT (runGuider (f err)) context)

-- | Run a 'Guider' to get the 'Handler' type that Servant expects.
guiderToHandler :: Context -> Logger -> Guider a -> Handler a
guiderToHandler context logger (Guider m) =
  Handler $ ExceptT $ try $ runLoggerT logger $ runReaderT m context
