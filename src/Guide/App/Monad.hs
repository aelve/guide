{- | Application monad for servant handlers. -}

module Guide.App.Monad
       ( GuideM (..)
       ) where

import Imports

import Guide.App.Error (AppError)
import Guide.Config (Config)


newtype GuideM a = GuideM
    { runGuideM :: ReaderT Config IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance MonadError AppError GuideM where
    throwError :: AppError -> GuideM a
    throwError = liftIO . throwIO

    catchError :: GuideM a -> (AppError -> GuideM a) -> GuideM a
    catchError = error "to be implemented"
