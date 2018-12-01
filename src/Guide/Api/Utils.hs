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
  , unH
  , BranchTag
  , RequestDetails(..)
  ) where


import Imports

import GHC.TypeLits
import GHC.Generics
import Data.Aeson
import Data.Swagger hiding (fieldLabelModifier)
import Data.Swagger.Internal.Schema
import Servant
import Servant.Swagger
import Network.HTTP.Types.Header (hReferer,hUserAgent)
import Servant.API.Modifiers (unfoldRequestArgument)
import Network.Wai (Request, requestHeaders)
import Servant.Server.Internal (DelayedIO, withRequest, addHeaderCheck, delayedFailFatal)


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
  deriving (Eq, Generic, Show)

instance ToJSON field => ToJSON (field ? help) where
  toJSON (H a) = toJSON a

instance FromJSON field => FromJSON (field ? help) where
    parseJSON f = H <$> parseJSON f

instance (KnownSymbol help, ToSchema a) => ToSchema (a ? help) where
  declareNamedSchema _ = do
    NamedSchema _ s <- declareNamedSchema (Proxy @a)
    return $ NamedSchema Nothing (s & description ?~ toText desc)
    where
      desc = symbolVal (Proxy @help)

instance {-# OVERLAPPING #-} (KnownSymbol help, Selector s, ToSchema c) => GToSchema (S1 s (K1 i (Maybe c ? help))) where
  gdeclareNamedSchema opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s (K1 i (Maybe c ? help))) False

-- | Unwrapper for @field '?' help@
unH :: forall field help . (field ? help) -> field
unH (H field) = field

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


-- | Servant request details, can be captured by adding `RequestDetails :>` to an API branch.
data RequestDetails = RequestDetails
  { rdIp      :: Text
  , rdReferer :: Text
  , rdUA      :: Text
  } deriving (Show, Generic)

instance (HasServer api context)
  => HasServer (Maybe RequestDetails :> api) context where

  type ServerT (Maybe RequestDetails :> api) m = Maybe RequestDetails -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver = route (Proxy :: Proxy api) context $
      subserver `addHeaderCheck` withRequest headerCheck
    where
      headerCheck :: Request -> DelayedIO (Maybe RequestDetails)
      headerCheck req = unfoldRequestArgument (Proxy :: Proxy '[Optional, Strict]) errReq errSt meRequestDatails
        where
          meRequestDatails :: Maybe (Either Text RequestDetails)
          meRequestDatails = getCompose $ liftA2 RequestDetails (Compose ip) (Compose referer) <*> Compose ua

          ip :: FromHttpApiData a => Maybe (Either Text a)
          ip = fmap parseHeader $ lookup "X-Forwarded-For" (requestHeaders req)
          referer :: FromHttpApiData a => Maybe (Either Text a)
          referer = fmap parseHeader $ lookup hReferer (requestHeaders req)
          ua :: FromHttpApiData a => Maybe (Either Text a)
          ua = fmap parseHeader $ lookup hUserAgent (requestHeaders req)

          errReq = delayedFailFatal err400
            { errBody = "One of Headers: " <> "Referer, X-Forwarded-For or User-Agent" <> " is required" }
          errSt e = delayedFailFatal err400
            { errBody = "Error parsing one of headers "
           <> "Referer, X-Forwarded-For or User-Agent"
           <> " failed: "
           <> toLByteString e
            }
