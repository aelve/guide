{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | 'Guider' monad with 'Config' to replace servant's 'Handler'. -}
module Guide.Api.Guider
       ( Guider (..)
       , GuiderServer
       , ConfigHub (..)
       , guiderToHandler
       ) where

import Imports

import Servant (Handler (..), ServantErr)
import Servant.Server.Generic

import Guide.Config (Config)
import Guide.Api.Utils (RequestDetails)
import Guide.State (DB)


-- | Custom 'Guider' type holds the 'Config' always on hand.
newtype Guider a = Guider
  { runGuider :: ReaderT ConfigHub IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ConfigHub)

-- | Context of Guider
data ConfigHub = ConfigHub
  { chConfig  :: Config
  , chDB      :: DB
  , chDetails :: RequestDetails
  }

instance MonadError ServantErr Guider where
  throwError :: ServantErr -> Guider a
  throwError = liftIO . throwIO

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f = Guider $ ReaderT $ \configHub ->
    runReaderT m configHub `catch` (\err -> runReaderT (runGuider (f err)) configHub)

-- | The custom type won't be accepted by servant server without this conventor used with 'hoistServer'.
guiderToHandler :: ConfigHub -> Guider a -> Handler a
guiderToHandler configHub (Guider m) = Handler $ ExceptT $ try $ runReaderT m configHub

-- | 'GuiderServer' used to create 'Guider' api.
type GuiderServer = AsServerT Guider
