{-# LANGUAGE OverloadedStrings #-}


--
-- Connection methods for postgres database
--
module Guide.Database.Connection
       ( connect
       , run'
       ) where

import Imports
import Hasql.Connection (Connection, Settings)
import Hasql.Session (Session)

import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS


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
run' :: Session a -> Connection -> IO a
run' s c = either (error . show) pure =<< HS.run s c
