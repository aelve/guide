module Guide.Api.Utils
  ( jsonOptions
  , schemaOptions
  ) where

import Imports

import Data.Aeson
import Data.Swagger (SchemaOptions, fromAesonOptions)

-- | Nice JSON options.
--
-- @ciConsDeleted@ becomes @cons_deleted@. Underscores at the end are
-- dropped (useful for fields like @categoryGroup_@).
jsonOptions :: Options
jsonOptions = defaultOptions{ fieldLabelModifier = camelTo2 '_' . trim }
  where
    trim :: String -> String
    trim = dropWhileEnd (== '_') . dropWhile (not . isUpper)

-- | Swagger schema-generating options that match 'jsonOptions'.
schemaOptions :: SchemaOptions
schemaOptions = fromAesonOptions jsonOptions
