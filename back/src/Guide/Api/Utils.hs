{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Guide.Api.Utils
(
  jsonOptions,
  schemaOptions,
  BranchTag,
  RequestDetails(..),
  field,
  inlineSchema,
)
where


import Imports

import GHC.TypeLits
import Data.IP (IP)
import Data.Aeson
import Data.Swagger hiding (format, fieldLabelModifier)
import Servant
import Servant.Swagger
import Network.HTTP.Types.Header (hReferer,hUserAgent)
import Network.Wai (Request, requestHeaders, remoteHost)
import Servant.Server.Internal (DelayedIO, withRequest, addHeaderCheck)

import Guide.Utils (sockAddrToIP)

import qualified Network.HTTP.Types.Header as NHTH
import qualified Data.Text as T
import qualified Data.HashMap.Strict.InsOrd as InsOrd

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

-- | Nice JSON options.
--
-- @ciConsDeleted@ becomes @cons_deleted@.
jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropWhile (not . isUpper)
  }

-- | Swagger schema-generating options that match 'jsonOptions'.
schemaOptions :: SchemaOptions
schemaOptions = fromAesonOptions jsonOptions

----------------------------------------------------------------------------
-- BranchTag
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- RequestDetails
----------------------------------------------------------------------------

-- | Servant request details, can be captured by adding @RequestDetails :>@ to an API branch.
data RequestDetails = RequestDetails
  { rdIp        :: Maybe IP    -- ^ request ip address
  , rdReferer   :: Maybe Text  -- ^ request referrer
  , rdUserAgent :: Maybe Text  -- ^ request User-Agent
  } deriving (Show, Generic)

instance FromHttpApiData IP where
  parseUrlPiece text = either (Left . toText) Right $ readEither $ toString text

instance (HasSwagger api) => HasSwagger (RequestDetails :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)

instance (HasServer api context) => HasServer (RequestDetails :> api) context where

  type ServerT (RequestDetails :> api) m = RequestDetails -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver = route (Proxy :: Proxy api) context $
      subserver `addHeaderCheck` withRequest getRequestDetails
    where
      getRequestDetails :: Request -> DelayedIO RequestDetails
      getRequestDetails req = pure $ RequestDetails mIp mReferer mUseragent
        where
          mIp :: Maybe IP
          mIp =  getIp $ lookupName "Forwarded-For" <|> lookupName "X-Forwarded-For"
          mReferer :: Maybe Text
          mReferer = getHeader hReferer
          mUseragent :: Maybe Text
          mUseragent = getHeader hUserAgent

          lookupName :: NHTH.HeaderName -> Maybe ByteString
          lookupName headerName = lookup headerName (requestHeaders req)

          getHeader :: FromHttpApiData a => NHTH.HeaderName -> Maybe a
          getHeader headerName =
            (either (\_ -> Nothing) Just) . parseHeader =<<
            lookupName headerName

          getIp :: Maybe ByteString -> Maybe IP
          getIp mBody = case mBody of
            Nothing -> sockAddrToIP $ remoteHost req
            Just ff -> readMaybe (toString ip)
              where
                addr = T.strip . snd . T.breakOnEnd "," $ utf8ToText ff
                ip -- [IPv6]:port
                  | T.take 1 addr == "[" =
                    T.drop 1 (T.takeWhile (/= ']') addr)
                   -- IPv4 or IPv4:port
                  | T.any (== '.') addr =
                    T.takeWhile (/= ':') addr
                   -- IPv6 without port
                  | otherwise = addr

----------------------------------------------------------------------------
-- Schema lenses
----------------------------------------------------------------------------

-- | A lens for fields in Swagger schemas.
--
-- Throws an 'error' if the field does not exist.
field
  :: HasCallStack
  => Text -> Lens' (InsOrd.InsOrdHashMap Text (Referenced Schema)) (Referenced Schema)
field k f m = case InsOrd.lookup k m of
  Just v -> f v <&> \v' -> InsOrd.insert k v' m
  Nothing -> error $ format "field: field {} not found" (show k)

-- | A lens for inline schemas.
--
-- Throws an 'error' if the field is a reference to another schema (i.e. not
-- a primitive field like 'Text', 'Int', list, etc). In this case you can
-- first generate the schema, e.g. with @toSchema (Proxy \@SomeType)@.
inlineSchema
  :: HasCallStack
  => Lens' (Referenced Schema) Schema
inlineSchema f (Inline v) =
  Inline <$> f v
inlineSchema _ (Ref r) =
  error $ format "inlineSchema: expected Inline, got {}" (show r)
