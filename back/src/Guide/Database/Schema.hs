{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


-- | Schemas to create table for guide database
module Guide.Database.Schema
(
  setupDatabase,
)
where

import Imports

import Hasql.Session (Session)
import Hasql.Statement (Statement (..))
import Data.Profunctor (lmap)

import qualified Hasql.Session as HS

import Guide.Database.Utils
import Guide.Database.Connection (connect, runSession)


-- | List of all migrations.
migrations :: [(Int32, Session ())]
migrations =
  [ (0, v0)
  ]

-- | Prepare the database for use by Guide.
--
-- Determines which migrations have to be run, and runs them. Errors out if
-- any migrations fail.
--
-- Note: 'setupDatabase' uses @"guide"@ as the database name, but it does
-- not create a database if it does not exist yet. You should create the
-- database manually by doing @CREATE DATABASE guide;@ or run Postgres with
-- @POSTGRES_DB=guide@ when running when running the app for the first time.
--
-- TODO: check schema hash as well, not just schema version?
setupDatabase :: IO ()
setupDatabase = do
  conn <- connect
  mbSchemaVersion <- runSession conn getSchemaVersion
  case mbSchemaVersion of
    Nothing -> formatLn "No schema found. Creating tables and running all migrations."
    Just v  -> formatLn "Schema version is {}." v
  let schemaVersion = fromMaybe (-1) mbSchemaVersion
  let neededMigrations =
        filter
          (\(migrationVersion, _) -> migrationVersion > schemaVersion)
          migrations
  if null neededMigrations then
    putStrLn "Schema is up to date."
  else do
    putStrLn "Schema is not up to date, running migrations."
    for_ neededMigrations $ \(migrationVersion, migration) -> do
      format "Migration {}: " migrationVersion
      runSession conn (migration >> setSchemaVersion migrationVersion)
      formatLn "done."

----------------------------------------------------------------------------
-- Schema version table
----------------------------------------------------------------------------

-- | Get schema version (i.e. the version of the last migration that was
-- run).
--
-- If the @schema_version@ table doesn't exist, creates it.
getSchemaVersion :: Session (Maybe Int32)
getSchemaVersion = do
  HS.statement () $
    [execute|
      CREATE TABLE IF NOT EXISTS schema_version (
        name text PRIMARY KEY,
        version integer
      );
      INSERT INTO schema_version (name, version)
        VALUES ('main', null)
        ON CONFLICT DO NOTHING;
    |]
  let selectVersion :: Statement () (Maybe (SingleColumn (Maybe Int32)))
      selectVersion =
        [queryRowMaybe|
          SELECT version FROM schema_version WHERE name = 'main'
        |]
  HS.statement () selectVersion >>= \case
    Just (SingleColumn (Just v)) -> pure (Just v)
    _ -> pure Nothing

-- | Set schema version.
--
-- Assumes the @schema_version@ table exists.
setSchemaVersion :: Int32 -> Session ()
setSchemaVersion version = do
  let statement :: Statement Int32 ()
      statement = lmap SingleParam $
        [execute|
          UPDATE schema_version SET version = $1 WHERE name = 'main'
        |]
  HS.statement version statement

----------------------------------------------------------------------------
-- Version 0
----------------------------------------------------------------------------

-- | Schema version 0: initial schema.
v0 :: Session ()
v0 = do
  v0_createTypeProCon
  v0_createTableCategories
  v0_createTableItems
  v0_createTableTraits
  v0_createTableUsers
  v0_createTablePendingEdits

-- | Create an enum type for trait type ("pro" or "con").
v0_createTypeProCon :: Session ()
v0_createTypeProCon = HS.statement () $
  [execute|
    CREATE TYPE trait_type AS ENUM ('pro', 'con');
  |]

-- | Create table @traits@, corresponding to 'Guide.Types.Core.Trait'.
v0_createTableTraits :: Session ()
v0_createTableTraits = HS.statement () $
  [execute|
    CREATE TABLE traits (
      uid text PRIMARY KEY,           -- Unique trait ID
      content text NOT NULL,          -- Trait content as Markdown
      deleted boolean                 -- Whether the trait is deleted
        NOT NULL,
      type_ trait_type NOT NULL,      -- Trait type (pro or con)
      item_uid text                   -- Item that the trait belongs to
        REFERENCES items (uid)
        ON DELETE CASCADE
    );
  |]

-- | Create table @items@, corresponding to 'Guide.Types.Core.Item'.
v0_createTableItems :: Session ()
v0_createTableItems = HS.statement () $
  [execute|
    CREATE TABLE items (
      uid text PRIMARY KEY,           -- Unique item ID
      name text NOT NULL,             -- Item title
      created timestamptz NOT NULL,   -- When the item was created
      link text,                      -- Optional URL
      hackage text,                   -- Package name on Hackage
      summary text NOT NULL,          -- Item summary as Markdown
      ecosystem text NOT NULL,        -- The ecosystem section
      notes text NOT NULL,            -- The notes section
      deleted boolean                 -- Whether the item is deleted
        NOT NULL,
      category_uid text               -- Category that the item belongs to
        REFERENCES categories (uid)
        ON DELETE CASCADE,
      pros_order text[]               -- Uids of item's pro traits; this list specifies
        NOT NULL,                     --   in what order they should be displayed, and
                                      --   is necessary to allow moving traits up and
                                      --   down
      cons_order text[]               -- Uids of item's con traits
        NOT NULL
    );
  |]

-- | Create table @categories@, corresponding to 'Guide.Types.Core.Category'.
v0_createTableCategories :: Session ()
v0_createTableCategories = HS.statement () $
  [execute|
    CREATE TABLE categories (
      uid text PRIMARY KEY,           -- Unique category ID
      title text NOT NULL,            -- Category title
      created timestamptz NOT NULL,   -- When the category was created
      group_ text NOT NULL,           -- "Grandcategory"
      status text NOT NULL,           -- Category status ("in progress", etc); the list
                                      --   of possible statuses is defined by backend
      notes text NOT NULL,            -- Category notes as Markdown
      enabled_sections text[]         -- Item sections to show to users; the list of
        NOT NULL,                     --   possible section names is defined by backend
      items_order text[]              -- Uids of items in the category; this list
        NOT NULL                      --   specifies in what order they should be
                                      --   displayed, and is necessary to allow moving
                                      --   items up and down
    );
  |]

-- | Create table @users@, storing user data.
v0_createTableUsers :: Session ()
v0_createTableUsers = HS.statement () $
  [execute|
    CREATE TABLE users (
      uid text PRIMARY KEY,           -- Unique user ID
      name text NOT NULL,             -- User name
      email text NOT NULL,            -- User email
      password_scrypt text,           -- User password (scrypt-ed)
      is_admin boolean                -- Whether the user is admin
        DEFAULT false
        NOT NULL
    );
  |]

-- | Create table @pending_edits@, storing users' edits and metadata about
-- them (who made the edit, when, etc).
v0_createTablePendingEdits :: Session ()
v0_createTablePendingEdits = HS.statement () $
  [execute|
    CREATE TABLE pending_edits (
      uid bigserial PRIMARY KEY,      -- Unique id
      edit json NOT NULL,             -- Edit in JSON format
      ip inet,                        -- IP address of edit maker
      time_ timestamptz NOT NULL      -- When the edit was created
    );
  |]
