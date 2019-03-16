{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}


module To (
  toText,
  toLText,
  toByteString,
  toLByteString,
  toString,
  toBuilder,
)
where


import Prelude

import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.Lazy.Builder (Builder)

import qualified Data.Text.Lazy.Builder as B

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import qualified Data.ByteString.UTF8 as UTF8


class ToText t where
    -- | Transforming to strict Text
    toText :: t -> Text
instance (a ~ Char) => ToText [a] where
    toText = pack
    {-# INLINE toText #-}
instance ToText TL.Text where
    toText = TL.toStrict
    {-# INLINE toText #-}
instance ToText Builder where
    toText = TL.toStrict . B.toLazyText
    {-# INLINE toText #-}
instance ToText BS.ByteString where
    toText = decodeUtf8With lenientDecode
    {-# INLINE toText #-}
instance ToText BSL.ByteString where
    toText = decodeUtf8With lenientDecode . BSL.toStrict
    {-# INLINE toText #-}

class ToLText t where
    -- | Transforming to lazy Text
    toLText :: t -> TL.Text
instance (a ~ Char) => ToLText [a] where
    toLText = TL.pack
    {-# INLINE toLText #-}
instance ToLText Text where
    toLText = TL.fromStrict
    {-# INLINE toLText #-}
instance ToLText Builder where
    toLText = B.toLazyText
    {-# INLINE toLText #-}
instance ToLText BS.ByteString where
    toLText = TL.fromStrict . decodeUtf8With lenientDecode
    {-# INLINE toLText #-}
instance ToLText BSL.ByteString where
    toLText = TL.decodeUtf8With lenientDecode
    {-# INLINE toLText #-}

class ToBuilder t where
    -- | Transforming to Builder.
    toBuilder :: t -> Builder
instance (a ~ Char) => ToBuilder [a] where
    toBuilder = B.fromString
    {-# INLINE toBuilder #-}
instance ToBuilder Text where
    toBuilder = B.fromText
    {-# INLINE toBuilder #-}
instance ToBuilder TL.Text where
    toBuilder = B.fromLazyText
    {-# INLINE toBuilder #-}
instance ToBuilder BS.ByteString where
    toBuilder = B.fromText . decodeUtf8With lenientDecode
    {-# INLINE toBuilder #-}
instance ToBuilder BSL.ByteString where
    toBuilder = B.fromLazyText . TL.decodeUtf8With lenientDecode
    {-# INLINE toBuilder #-}

class ToString t where
    -- | Transforming to String
    toString :: t -> String
instance ToString Text where
    toString = unpack
    {-# INLINE toString #-}
instance ToString TL.Text where
    toString = TL.unpack
    {-# INLINE toString #-}
instance ToString Builder where
    toString = TL.unpack . B.toLazyText
    {-# INLINE toString #-}
instance ToString BS.ByteString where
    toString = UTF8.toString
    {-# INLINE toString #-}
instance ToString BSL.ByteString where
    toString = UTF8L.toString
    {-# INLINE toString #-}

class ToByteString t where
    -- | Transforming to strict ByteString
    toByteString :: t -> BS.ByteString
instance ToByteString Text where
    toByteString = encodeUtf8
    {-# INLINE toByteString #-}
instance ToByteString TL.Text where
    toByteString = encodeUtf8 . TL.toStrict
    {-# INLINE toByteString #-}
instance ToByteString Builder where
    toByteString = encodeUtf8 . TL.toStrict . B.toLazyText
    {-# INLINE toByteString #-}
instance (a ~ Char) => ToByteString [a] where
    toByteString = UTF8.fromString
    {-# INLINE toByteString #-}
instance ToByteString BSL.ByteString where
    toByteString = BSL.toStrict
    {-# INLINE toByteString #-}

class ToLByteString t where
    -- | Transforming to lazy ByteString
    toLByteString :: t -> BSL.ByteString
instance ToLByteString Text where
    toLByteString = TL.encodeUtf8 . TL.fromStrict
    {-# INLINE toLByteString #-}
instance ToLByteString TL.Text where
    toLByteString = TL.encodeUtf8
    {-# INLINE toLByteString #-}
instance ToLByteString Builder where
    toLByteString = TL.encodeUtf8 . B.toLazyText
    {-# INLINE toLByteString #-}
instance (a ~ Char) => ToLByteString [a] where
    toLByteString = UTF8L.fromString
    {-# INLINE toLByteString #-}
instance ToLByteString BS.ByteString where
    toLByteString = BSL.fromStrict
    {-# INLINE toLByteString #-}
