module Guide.Api.Guider
       ( Guider
       , guiderToHandler
       ) where

import Imports

import Control.Monad.IO.Class (MonadIO)
import Servant (Handler)
import Servant.Server.Internal.ServantErr (ServantErr)

import Guide.Config (Config (..), def)


newtype Guider a = Guider
  { runGuider :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)


instance MonadError ServantErr Guider where
  throwError :: ServantErr -> Guider a
  throwError = liftIO . throwIO

  catchError :: Guider a -> (ServantErr -> Guider a) -> Guider a
  catchError = error "There is no implementation for catchError"

guiderToHandler :: Guider a -> Handler a
guiderToHandler guider = liftIO $ runReaderT (runGuider guider) def
