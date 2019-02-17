{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | 'Guider' monad with 'Config' to replace servant's 'Handler'.
module Guide.Api.Guider
(
  Guider (..),
  GuiderServer,
  Context (..),
  DefDi,
  guiderToHandler,
)
where

import Imports

import Df1
import Di.Monad (MonadDi, runDiT)
import Servant (Handler (..), ServantErr (..))
import Servant.Server.Generic

import Guide.Api.Utils (RequestDetails)
import Guide.Config (Config)
import Guide.Logger
import Guide.State (DB)

import qualified Control.Monad.Catch as Exc
import qualified Di


-- | A type for Guide handlers. Provides:
--
-- * Logging via 'DefDiT'
-- * Access to 'Context'
newtype Guider a = Guider
  { runGuider :: ReaderT Context DefDiT a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context, MonadDi Level Path Message, Exc.MonadThrow)

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
    if | code >= 500 -> errorT ("error: "+||err||+"")
       | otherwise -> debugT ("response code "+|code|+": "+|reason|+"")
    Exc.throwM err

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f = Guider $ ReaderT $ \context ->
    runReaderT m context `Exc.catch` (\err -> runReaderT (runGuider (f err)) context)

-- | The custom type won't be accepted by servant server without this
-- conventor used with 'hoistServer'.
guiderToHandler :: Context -> DefDi -> Guider a -> Handler a
guiderToHandler context di (Guider m) =
  Handler $ ExceptT $ try $ runDiT di $ runReaderT m context

-- | 'GuiderServer' used to create 'Guider' api.
type GuiderServer = AsServerT Guider
