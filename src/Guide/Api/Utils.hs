{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


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
import Data.IP (IP)
import GHC.Generics
import Data.Aeson
import Data.Swagger hiding (fieldLabelModifier)
import Data.Swagger.Internal.Schema
import Servant
import Servant.Swagger
import Network.HTTP.Types.Header (hReferer,hUserAgent)
import Network.Wai (Request, requestHeaders, remoteHost)
import Servant.Server.Internal (DelayedIO, withRequest, addHeaderCheck)

import Guide.Utils (sockAddrToIP)

import qualified Network.HTTP.Types.Header as NHTH
import qualified Data.Text as T


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

instance (HasServer api context)
  => HasServer (RequestDetails :> api) context where

  type ServerT (RequestDetails :> api) m = RequestDetails -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

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
          getHeader headerName = join $
            (either (\_ -> Nothing) Just) . parseHeader <$> lookupName headerName

          getIp :: Maybe ByteString -> Maybe IP
          getIp mBody = case mBody of
            Nothing -> sockAddrToIP $ remoteHost req
            Just ff -> case readMaybe (toString ip) of
              Nothing -> error $ "couldn't read Forwarded-For address: " ++
                show ip ++ " (full header: " ++ show ff ++ ")"
              Just i  -> pure i
              where
                addr = T.strip . snd . T.breakOnEnd "," $ toText ff
                ip -- [IPv6]:port
                  | T.take 1 addr == "[" =
                    T.drop 1 (T.takeWhile (/= ']') addr)
                   -- IPv4 or IPv4:port
                  | T.any (== '.') addr =
                    T.takeWhile (/= ':') addr
                   -- IPv6 without port
                  | otherwise = addr
