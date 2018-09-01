{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module Guide.Api.Utils
  ( jsonOptions
  , schemaOptions
  , type (?)(..)
  , BranchTag
  ) where


import Imports

import GHC.TypeLits
import Data.Aeson
import Data.Swagger hiding (fieldLabelModifier)
import Servant
import Servant.Swagger


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
    return $ NamedSchema Nothing (s & description ?~ toText desc)
    where
      desc = symbolVal (Proxy @help)

-- | A way to name branches of Swagger API.
--
-- Taken from <https://github.com/haskell-servant/servant-swagger/issues/73>
data BranchTag (name :: Symbol) (desc :: Symbol)

instance HasServer api ctx =>
         HasServer (BranchTag name desc :> api) ctx where
  type ServerT (BranchTag name desc :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance (HasSwagger api, KnownSymbol name, KnownSymbol desc) =>
         HasSwagger (BranchTag name desc :> api) where
  toSwagger _ =
    let tag =
          Tag (toText $ symbolVal (Proxy @name))
            ((\case
                "" -> Nothing
                t -> Just t) .
             toText $
             symbolVal (Proxy @desc))
            Nothing
     in toSwagger (Proxy @api) & applyTags [tag]
