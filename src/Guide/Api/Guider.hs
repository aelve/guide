{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | 'Guider' monad with 'Config' to replace servant's 'Handler'. -}
module Guide.Api.Guider
       ( Guider (..)
       , GuiderServer
       , guiderToHandler
       ) where

import Imports

import Servant (Handler (..), ServantErr)
import Servant.Server.Generic

import Guide.Config (Config (..), def)


-- | Custom 'Guider' type holds the 'Config' always on hand.
newtype Guider a = Guider
  { runGuider :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance MonadError ServantErr Guider where
  throwError :: ServantErr -> Guider a
  throwError = liftIO . throwIO

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f =
    liftIO $ (runReaderT m def) `catch` (\e -> runReaderT (runGuider (f e)) def)

-- | The custom type won't be accepted by servant server without this conventor using at 'hoistServer'.
guiderToHandler :: Guider a -> Handler a
guiderToHandler (Guider m) = liftIO (runReaderT m def)

-- | 'GuiderServer' used to create 'Guider' api.
type GuiderServer = AsServerT Guider
