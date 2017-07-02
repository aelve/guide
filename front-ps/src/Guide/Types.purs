module Guide.Types where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Lib.IsomorphicFetch (URI)

newtype User = User {
  login :: String,
  avatar_url :: URI,
  html_url :: URI
}

derive instance gUser :: Generic User _
derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User
instance showUser :: Show User where
  show = genericShow
instance decodeUser :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUser :: Encode User where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

type Users = Array User
