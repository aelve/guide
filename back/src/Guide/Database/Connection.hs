{-# LANGUAGE OverloadedStrings #-}


-- | Connect to the Guide database.
module Guide.Database.Connection
       ( connect
       , runSessionExceptT
       , runSession
       , runTransactionExceptT
       , runTransaction
       ) where

import Imports
import Hasql.Connection (Connection, Settings)
import Hasql.Session (Session)
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (Mode, IsolationLevel(..))

import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Transaction.Sessions as HT

import Guide.Database.Types (DatabaseError)


-- | Create a database connection (the destination is hard-coded for now).
--
-- Throws an 'error' if the connection could not be established.
connect :: IO Connection
connect = do
  HC.acquire connectionSettings >>= \case
    Left Nothing -> error "connect: unknown exception"
    Left (Just x) -> error ("connect: " ++ utf8ToString x)
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

-- | Run an @ExceptT Session@ against the given database connection,
-- throwing an 'error' in case of failure.
runSessionExceptT :: Connection -> ExceptT DatabaseError Session a -> IO a
runSessionExceptT connection session =
  unwrapRight =<< unwrapRight =<< HS.run (runExceptT session) connection

-- | Run a @Session@ against the given database connection, throwing an
-- 'error' in case of failure.
runSession :: Connection -> Session a -> IO a
runSession connection session =
  unwrapRight =<< HS.run session connection

-- | Run an @ExceptT Transaction@ against the given database connection,
-- throwing an 'error' in case of failure.
--
-- The transaction is ran with the strongest ('Serializable') isolation
-- level. Use 'HT.transaction' if you need a different isolation level.
runTransactionExceptT
  :: Connection -> Mode -> ExceptT DatabaseError Transaction a -> IO a
runTransactionExceptT connection mode transaction =
  unwrapRight =<< unwrapRight =<<
  HS.run (HT.transaction Serializable mode (runExceptT transaction)) connection

-- | Run a @Transaction@ against the given database connection, throwing an
-- 'error' in case of failure.
--
-- The transaction is ran with the strongest ('Serializable') isolation
-- level. Use 'HT.transaction' if you need a different isolation level.
runTransaction :: Connection -> Mode -> Transaction a -> IO a
runTransaction connection mode transaction =
  unwrapRight =<<
  HS.run (HT.transaction Serializable mode transaction) connection

-- | Unwrap 'Either', failing in case of 'Left'.
unwrapRight :: Show e => Either e a -> IO a
unwrapRight = either (error . show) pure
