{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Custom errors for servant handlers. -}

module Guide.App.Error
       ( WithError
       , AppError (..)
       , toHttpError
       ) where

import Imports

import Servant.Server (ServantErr, err401, err404, err417, err500, errBody)


-- | Type alias for errors.
type WithError m = MonadError AppError m

data AppError
    -- | General not found
    = NotFound
    -- | Some exceptional circumstance has happened stop execution and
    -- return. Optional text to provide some context in server logs
    | ServerError Text
    -- | A required permission level was not met. Optional text to provide some context.
    | NotAllowed Text
    -- | Given inputs do not conform to the expected format or shape. Optional
    -- text to provide some context in server logs
    | WrongArguments Text
    -- | An authentication header that was required was provided but not in a
    -- format that the server can understand
    | HeaderError Text
    deriving (Show, Eq)
    deriving anyclass (Exception)

toHttpError :: AppError -> ServantErr
toHttpError = \case
      NotFound           -> err404
      ServerError msg    -> err500 { errBody = toLByteString msg }
      NotAllowed msg     -> err401 { errBody = toLByteString msg }
      WrongArguments msg -> err417 { errBody = toLByteString msg }
      HeaderError name   -> err401 { errBody = toLByteString $ "Unable to decode header: " <> name }
