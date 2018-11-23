{- | Guider monad with Config to replace servant`s Handler. -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Guide.Api.Guider
       ( Guider (..)
       , GuiderServer
       , guiderToHandler
       ) where

import Imports

import Servant (Handler (..))
import Servant.Server.Generic
import Servant.Server.Internal.ServantErr (ServantErr)

import Guide.Config (Config (..), def)


-- | Guider type contains Config inside and replaces servant`s Handler type.
newtype Guider a = Guider
  { runGuider :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance MonadError ServantErr Guider where
  throwError :: ServantErr -> Guider a
  throwError = liftIO . throwIO

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError (Guider m) f =
    (Guider $ ReaderT $ \r -> (runReaderT m r)) `catchError`
      (\e -> Guider $ ReaderT $ \r -> (runReaderT (runGuider (f e)) r))

-- | Convertor from Guider to Handler
guiderToHandler :: Guider a -> Handler a
guiderToHandler (Guider m) = Handler $ ExceptT $ Right <$> runReaderT m def

-- | Server type for Guider
type GuiderServer = AsServerT Guider
