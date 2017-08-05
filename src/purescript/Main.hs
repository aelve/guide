{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import Prelude

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge (BridgeBuilder, BridgePart, Language( Haskell ), SumType,
                                    PSType, TypeInfo (..), (<|>), (^==), buildBridge,
                                    defaultBridge, typeName, mkSumType, writePSTypes)
import Language.PureScript.Bridge.PSTypes (psString)
import Guide.Api.ClientTypes (CCategoryDetail, CCategoryOverview, CGrandCategory, CItem, CTrait, CMarkdown)
import Guide.Types.Hue (Hue)
import Guide.Types.Core (CategoryStatus, ItemKind)

path :: FilePath
path = "front-ps/src/Generated"

psPosixTime :: PSType
psPosixTime = TypeInfo "" "Data.Time.NominalDiffTime" "NominalDiffTime" []

posixTimeBridge :: BridgeBuilder PSType
posixTimeBridge =
  typeName ^== "NominalDiffTime" >> pure psPosixTime

uidBridge :: BridgePart
uidBridge = typeName ^== "Uid" >> pure psString

byteStringBridge :: BridgePart
byteStringBridge = typeName ^== "ByteString" >> pure psString

-- TODO (sectore) Can we use PureScript's `Data.Date` here?
utcTimeBridge :: BridgePart
utcTimeBridge = typeName ^== "UTCTime" >> pure psString


bridge :: BridgeBuilder PSType
bridge = defaultBridge
  <|> posixTimeBridge
  <|> uidBridge
  <|> byteStringBridge
  <|> utcTimeBridge

clientTypes :: [SumType  'Haskell]
clientTypes =
  [ mkSumType (Proxy :: Proxy CGrandCategory)
  , mkSumType (Proxy :: Proxy CCategoryDetail)
  , mkSumType (Proxy :: Proxy CCategoryOverview)
  , mkSumType (Proxy :: Proxy Hue)
  , mkSumType (Proxy :: Proxy CategoryStatus)
  , mkSumType (Proxy :: Proxy CItem)
  , mkSumType (Proxy :: Proxy ItemKind)
  , mkSumType (Proxy :: Proxy CTrait)
  , mkSumType (Proxy :: Proxy CMarkdown)
  ]

-- FIXME: Currently `Uid a` defined in `Guide.Utils` is bridged into a `String`.
-- For example: `Uid Category` on Haskell side is bridged to `String`
-- It would be better to bridge it to a similar `Uid a` type

main :: IO ()
main = writePSTypes path (buildBridge bridge) clientTypes
