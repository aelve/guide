{-# LANGUAGE OverloadedStrings #-}


-- | Connect to the Guide database.
module Guide.Database.Connection
       ( connect
       , runSessionExceptT
       , runSession
       ) where

import Hasql.Connection (Connection, Settings)
import Hasql.Session (Session)
import Imports

import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS

import Guide.Database.Types (DatabaseError)


-- | Create a database connection (the destination is hard-coded for now).
--
-- Throws an 'error' if the connection could not be established.
connect :: IO Connection
connect = do
  HC.acquire connectionSettings >>= \case
    Left Nothing -> error "connect: unknown exception"
    Left (Just x) -> error ("connect: " ++ toString x)
    Right conn -> pure conn

-- | Connection settings
connectionSettings :: Settings
connectionSettings = HC.settings "localhost" 5432 dbUser dbPass dbName

-- | Database user
dbUser :: ByteString
dbUser = "postgres"

-- | Database password
dbPass :: ByteString
dbPass = "3"

-- | Database name
dbName :: ByteString
dbName = "guide"

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Like 'HS.run', but errors out in case of failure.
--
-- For @ExceptT Session@.
runSessionExceptT :: ExceptT DatabaseError Session a -> Connection -> IO a
runSessionExceptT s c = unwrapRight =<< unwrapRight =<< HS.run (runExceptT s) c

-- | Like 'HS.run', but errors out in case of failure.
--
-- For @Session@.
runSession :: Session a -> Connection -> IO a
runSession s c = unwrapRight =<< HS.run s c

-- | Unwrap 'Either', failing in case of 'Left'.
unwrapRight :: Show e => Either e a -> IO a
unwrapRight = either (error . show) pure
