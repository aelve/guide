{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Guide.Db.Schema
(
  setupDatabase,
)
where

import Imports
import Guide.Db.Connection (connect, run')

import Hasql.Session (Session)
import NeatInterpolation
import Hasql.Statement (Statement (..))

import qualified Hasql.Session as HS
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD


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
  mbSchemaVersion <- run' getSchemaVersion conn
  case mbSchemaVersion of
    Nothing -> formatLn "No schema found. Creating tables and running all migrations."
    Just v  -> formatLn "Schema version is {}." v
  let schemaVersion = fromMaybe (-1) mbSchemaVersion
  for_ migrations $ \(migrationVersion, migration) ->
    when (migrationVersion > schemaVersion) $ do
      format "Migration {}: " migrationVersion
      run' (migration >> setSchemaVersion migrationVersion) conn
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
  HS.sql $ toByteString [text|
    CREATE TABLE IF NOT EXISTS schema_version (
      name text PRIMARY KEY,
      version integer
    );
    INSERT INTO schema_version (name, version)
      VALUES ('main', null)
      ON CONFLICT DO NOTHING;
    |]
  let sql = "SELECT (version) FROM schema_version WHERE name = 'main'"
      encoder = HE.noParams
      decoder = HD.singleRow (HD.column (HD.nullable HD.int4))
  HS.statement () (Statement sql encoder decoder False)

-- | Set schema version.
--
-- Assumes the @schema_version@ table exists.
setSchemaVersion :: Int32 -> Session ()
setSchemaVersion version = do
  let sql = "UPDATE schema_version SET version = $1 WHERE name = 'main'"
      encoder = HE.param (HE.nullable HE.int4)
      decoder = HD.noResult
  HS.statement (Just version) (Statement sql encoder decoder True)

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
v0_createTypeProCon = HS.sql $ toByteString [text|
  CREATE TYPE trait_type AS ENUM ('pro', 'con');
  |]

-- | Create table @traits@, corresponding to 'Guide.Types.Core.Trait'.
v0_createTableTraits :: Session ()
v0_createTableTraits = HS.sql $ toByteString [text|
  CREATE TABLE traits (
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

-- | Create table @items@, corresponding to 'Guide.Types.Core.Item'.
v0_createTableItems :: Session ()
v0_createTableItems = HS.sql $ toByteString [text|
  CREATE TABLE items (
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

-- | Create table @categories@, corresponding to 'Guide.Types.Core.Category'.
v0_createTableCategories :: Session ()
v0_createTableCategories = HS.sql $ toByteString [text|
  CREATE TABLE categories (
    uid text PRIMARY KEY,           -- Unique category ID
    title text NOT NULL,            -- Category title
    created timestamp NOT NULL,     -- When the category was created
    group_ text NOT NULL,           -- "Grandcategory"
    status_ text NOT NULL,          -- Category status ("in progress", etc); the list of
                                    --   possible statuses is defined by backend
    notes text NOT NULL,            -- Category notes as Markdown
    enabled_sections text[]         -- Item sections to show to users; the list of
      NOT NULL                      --   possible section names is defined by backend
  );
  |]

-- | Create table @users@, storing user data.
v0_createTableUsers :: Session ()
v0_createTableUsers = HS.sql $ toByteString [text|
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
v0_createTablePendingEdits = HS.sql $ toByteString [text|
  CREATE TABLE pending_edits (
    uid bigserial PRIMARY KEY,      -- Unique id
    edit json NOT NULL,             -- Edit in JSON format
    ip inet,                        -- IP address of edit maker
    time_ timestamp NOT NULL        -- When the edit was created
  );
  |]
