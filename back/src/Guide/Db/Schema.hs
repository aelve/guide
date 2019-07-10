{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Guide.Db.Schema where

import Imports

import Hasql.Session (Session)
import NeatInterpolation
import Data.Either (lefts)
import System.IO (stderr, hPrint)
import Hasql.Connection (Connection, Settings)

import qualified Hasql.Session as HS
import qualified Hasql.Connection as HC


-- | It runs creation tables at first time. If table name exists it skips it.
-- If something returns an error it fails.
-- Damp base to check it has correct tables fields and types.
-- Check the migration version of data is correct also.
runCreateTables :: IO ()
runCreateTables = do
  conn <- connection
  createTables conn

-- | Throw error if connection lost
connection :: IO Connection
connection = do
  either (error . show) pure =<< HC.acquire connectionSettings

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

-- | Create tables and fails if something returns an error.
createTables :: Connection -> IO ()
createTables conn = do
  -- ToDo: check if tables exist with proper name, field and types.
  result <- mapM (\s -> HS.run s conn) sessionList
  let errors = lefts result
  unless (null errors) $ do
    mapM_ (hPrint stderr) errors
    fail "createTables failed"
  putStrLn "Tables was created if they not exist."

-- | List of all session
sessionList :: [Session ()]
sessionList =
  [ createTypeProCon
  , createTableCategories
  , createTableItems
  , createTableTraits
  , createTableUsers
  , createTableEdits
  ]

-- | Drop if exists and then create type pro/con. It used in trait.
createTypeProCon :: Session ()
createTypeProCon = HS.sql $ toByteString [text|
  DROP TYPE IF EXISTS trait_type CASCADE;
  CREATE TYPE trait_type AS ENUM ('pro', 'con');
  |]

-- | Create traits table if not exists
createTableTraits :: Session ()
createTableTraits = HS.sql $ toByteString [text|
  CREATE TABLE IF NOT EXISTS traits (
    uid text PRIMARY KEY,           -- Unique trait ID
    content text NOT NULL,          -- Trait content as Markdown
      deleted boolean               -- Whether the trait is deleted
      DEFAULT false
      NOT NULL,
    type_ trait_type NOT NULL,      -- Trait type (pro or con)
    item_uid text                   -- Item that the trait belongs to
      REFERENCES items (uid)
      ON DELETE CASCADE
  );
  |]

-- | Create items table if not exists
createTableItems :: Session ()
createTableItems = HS.sql $ toByteString [text|
  CREATE TABLE IF NOT EXISTS items (
    uid text PRIMARY KEY,           -- Unique item ID
    name text NOT NULL,             -- Item title
    created timestamp NOT NULL,     -- When the item was created
    group_ text,                    -- Optional group
    link text,                      -- Optional URL
    hackage text,                   -- Package name on Hackage
    summary text NOT NULL,          -- Item summary as Markdown
    ecosystem text NOT NULL,        -- The ecosystem section
    notes text NOT NULL,            -- The notes section
    deleted boolean                 -- Whether the item is deleted
      DEFAULT false
      NOT NULL,
    category_uid text               -- Category that the item belongs to
      REFERENCES categories (uid)
      ON DELETE CASCADE
  );
  |]

-- | Create table for categories
createTableCategories :: Session ()
createTableCategories = HS.sql $ toByteString [text|
  CREATE TABLE IF NOT EXISTS categories (
    uid text PRIMARY KEY,           -- Unique category ID
    title text NOT NULL,            -- Category title
    created timestamp NOT NULL,     -- When the category was created
    group_ text NOT NULL,           -- "Grandcategory"
    status_ text NOT NULL,          -- Category status ("in progress", etc); the list of
                                    --   possible statuses is defined by backend
    notes text NOT NULL,            -- Category notes as Markdown
    enabled_sections text[]         -- Item sections to show to users; the list of possible
      NOT NULL                      --   section names is defined by backend
  );
  |]

-- | Create table to store user's data
createTableUsers :: Session ()
createTableUsers = HS.sql $ toByteString [text|
  CREATE TABLE IF NOT EXISTS users (
    uid text PRIMARY KEY,           -- Unique user ID
    name text NOT NULL,             -- User name
    email text NOT NULL,            -- User email
    password_scrypt text,           -- User password (scrypt-ed)
    is_admin boolean                -- Whether the user is admin
      DEFAULT false
      NOT NULL
  );
  |]

-- | Create table to store edits and who made it
createTableEdits :: Session ()
createTableEdits = HS.sql $ toByteString [text|
  CREATE TABLE IF NOT EXISTS pending_edits (
    uid bigserial PRIMARY KEY,      -- Unique id
    edit json NOT NULL,             -- Edit in JSON format
    ip inet,                        -- IP address of edit maker
    time_ timestamp NOT NULL        -- When the edit was created
  );
  |]
