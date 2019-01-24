{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | 'Guider' monad with 'Config' to replace servant's 'Handler'. -}
module Guide.Api.Guider
       ( Guider (..)
       , GuiderServer
       , Context (..)
       , guiderToHandler
       ) where

import Imports

import Servant (Handler (..), ServantErr)
import Servant.Server.Generic

import qualified Control.Monad.Catch as Exc

import Guide.Api.Utils (RequestDetails)
import Guide.Config (Config)
import Guide.State (DB)
import Di.Core as Di (Di)
import Di.Monad as Di
import Df1

type DefDiT = DiT Level Text Text IO
type DefDi  = Di Level Text Text

-- | A type for Guide handlers. Provides access to everything in 'Context'.
newtype Guider a = Guider
  { runGuider :: ReaderT Context DefDiT a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

-- | Context of Guider
data Context = Context
  { cConfig  :: Config
  , cDB      :: DB
  , cDetails :: RequestDetails
  }

instance MonadError ServantErr Guider where
  throwError :: ServantErr -> Guider a
  throwError = liftIO . throwIO

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f = Guider $ ReaderT $ \context ->
    runReaderT m context `Exc.catch` (\err -> runReaderT (runGuider (f err)) context)

-- | The custom type won't be accepted by servant server without this conventor used with 'hoistServer'.
guiderToHandler :: Context -> DefDi -> Guider a -> Handler a
guiderToHandler context di (Guider m) =
  Handler $ ExceptT $ try $ runDiT di $ runReaderT m context

-- | 'GuiderServer' used to create 'Guider' api.
type GuiderServer = AsServerT Guider
