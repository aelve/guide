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
import Di.Monad (MonadDi, runDiT, DiT(..))
import Servant (Handler (..), ServantErr)
import Servant.Server.Generic

import Guide.Api.Utils (RequestDetails)
import Guide.Config (Config)
import Guide.Logger
import Guide.State (DB)

import qualified Control.Monad.Catch as Exc
import qualified Di
import qualified Di.Monad as DM
import qualified Di.Core as DC


-- | A type for Guide handlers. Provides access to everything in 'Context'.
newtype Guider a = Guider
  { runGuider :: ReaderT Context DefDiT a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context, MonadDi Level Path Message, Exc.MonadThrow)

-- | Context of Guider
data Context = Context
  { cConfig  :: Config
  , cDB      :: DB
  , cDetails :: RequestDetails
  }

instance MonadError ServantErr Guider where
  throwError :: ServantErr -> Guider a
  throwError err = Guider $ ReaderT $ \_ -> DM.diT $ \_ di -> DC.throw di err

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f = Guider $ ReaderT $ \context ->
    runReaderT m context `Exc.catch` (\err -> runReaderT (runGuider (f err)) context)

-- | The custom type won't be accepted by servant server without this conventor used with 'hoistServer'.
guiderToHandler :: Context -> DefDi -> Guider a -> Handler a
guiderToHandler context di (Guider m) =
  Handler $ ExceptT $ try $ runDiT di $ push "api" $ do
    Exc.catch
      (runReaderT m context)
      (\(err :: SomeException) ->
        Di.error (fromString $ "| (from Guider/Handler) " ++ show err) >> Exc.throwM err)

-- | 'GuiderServer' used to create 'Guider' api.
type GuiderServer = AsServerT Guider
