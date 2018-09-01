{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}


module Guide.Api.Utils
  ( jsonOptions
  , schemaOptions
  , type (?)(..)
  ) where


import Imports

import GHC.TypeLits
import Data.Aeson
import Data.Swagger hiding (fieldLabelModifier)
import qualified Data.Text.All as T


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

-- | A way to provide descriptions for record fields.
newtype (?) (field :: *) (help :: Symbol) = H field
  deriving (Generic, Show)

instance ToJSON field => ToJSON (field ? help) where
  toJSON (H a) = toJSON a

instance (KnownSymbol help, ToSchema a) => ToSchema (a ? help) where
  declareNamedSchema _ = do
    NamedSchema _ s <- declareNamedSchema (Proxy @a)
    return $ NamedSchema Nothing (s & description ?~ T.toStrict desc)
    where
      desc = symbolVal (Proxy @help)
