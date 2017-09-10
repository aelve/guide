{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import Prelude

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge ( BridgePart, Language( Haskell ), PSType,
                                    SumType, TypeInfo (..), (<|>),
                                    (^==), buildBridge, defaultBridge,
                                    typeName, mkSumType, writePSTypes)
import Language.PureScript.Bridge.PSTypes (psString)
import Language.PureScript.Bridge.TypeParameters (A)

import Guide.Api.Types (ApiError, CategoryInfo, CCategoryDetail, CItem, CMarkdown, CTrait, CUid)
import Guide.Types.Core (CategoryStatus, ItemKind)
import Guide.Types.Hue (Hue)

path :: FilePath
path = "front-ps/common/Generated"

psPosixTime :: PSType
psPosixTime = TypeInfo "" "Data.Time.NominalDiffTime" "NominalDiffTime" []

posixTimeBridge :: BridgePart
posixTimeBridge =
  typeName ^== "NominalDiffTime" >> pure psPosixTime

byteStringBridge :: BridgePart
byteStringBridge = typeName ^== "ByteString" >> pure psString

-- TODO (sectore) Can we use PureScript's `Data.Date` here?
utcTimeBridge :: BridgePart
utcTimeBridge = typeName ^== "UTCTime" >> pure psString

bridge :: BridgePart
bridge = defaultBridge
  <|> posixTimeBridge
  <|> byteStringBridge
  <|> utcTimeBridge

clientTypes :: [SumType  'Haskell]
clientTypes =
  [ mkSumType (Proxy @ApiError)
  , mkSumType (Proxy @CategoryStatus)
  , mkSumType (Proxy @CategoryInfo)
  , mkSumType (Proxy @CCategoryDetail)
  , mkSumType (Proxy @CItem)
  , mkSumType (Proxy @CTrait)
  , mkSumType (Proxy @CMarkdown)
  , mkSumType (Proxy @Hue)
  , mkSumType (Proxy @ItemKind)
  , mkSumType (Proxy @(CUid A))
  ]

-- FIXME: Currently `Uid a` defined in `Guide.Utils` is bridged into a `String`.
-- For example: `Uid Category` on Haskell side is bridged to `String`
-- It would be better to bridge it to a similar `Uid a` type

main :: IO ()
main = writePSTypes path (buildBridge bridge) clientTypes
