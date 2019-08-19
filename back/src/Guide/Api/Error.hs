{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Guide.Api.Error
(
  ErrorResponse,
)
where


import Imports

import Data.Swagger
import GHC.TypeLits
import Servant
import Servant.Swagger


-- Taken from https://github.com/haskell-servant/servant-swagger/issues/59
data ErrorResponse (code :: Nat) (description :: Symbol)

instance
  ( HasSwagger api
  , KnownNat code
  , KnownSymbol desc )
  => HasSwagger (ErrorResponse code desc :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & setResponse (fromInteger code) (return responseSchema)
    where
      code = natVal (Proxy :: Proxy code)
      desc = symbolVal (Proxy :: Proxy desc)
      responseSchema = mempty
        & description .~ toText desc

instance HasLink sub => HasLink (ErrorResponse code desc :> sub) where
    type MkLink (ErrorResponse code desc :> sub) a = MkLink sub a
    toLink f _ l = toLink f (Proxy :: Proxy sub) l

instance HasServer api ctx => HasServer (ErrorResponse code desc :> api) ctx where
  type ServerT (ErrorResponse code desc :> api) m = ServerT api m
  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s
