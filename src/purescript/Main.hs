{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import Prelude

import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge (BridgeBuilder, BridgePart, Language( Haskell ), SumType,
                                    PSType, TypeInfo (..), (<|>), (^==), buildBridge,
                                    defaultBridge, typeName, mkSumType, writePSTypes)
import Language.PureScript.Bridge.PSTypes (psString)
import Guide.Api.ClientTypes (CCategoryDetail, CGrandCategory, CCategoryOverview)
import Guide.Types.Hue (Hue)
import Guide.Types.Core (CategoryStatus, Item, ItemKind, Trait)

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

markdownBlockBridge :: BridgePart
markdownBlockBridge = typeName ^== "MarkdownBlock" >> pure psString

markdownTreeBridge :: BridgePart
markdownTreeBridge = typeName ^== "MarkdownTree" >> pure psString

markdownInlineBridge :: BridgePart
markdownInlineBridge = typeName ^== "MarkdownInline" >> pure psString

-- TODO (sectore) Can we use PureScript's `Data.Date` here?
utcTimeBridge :: BridgePart
utcTimeBridge = typeName ^== "UTCTime" >> pure psString

bridge :: BridgeBuilder PSType
bridge = defaultBridge
  <|> posixTimeBridge
  <|> uidBridge
  <|> byteStringBridge
  <|> utcTimeBridge
  <|> markdownBlockBridge
  <|> markdownTreeBridge
  <|> markdownInlineBridge

clientTypes :: [SumType  'Haskell]
clientTypes =
  [ mkSumType (Proxy :: Proxy CGrandCategory)
  , mkSumType (Proxy :: Proxy CCategoryDetail)
  , mkSumType (Proxy :: Proxy CCategoryOverview)
  , mkSumType (Proxy :: Proxy Hue)
  , mkSumType (Proxy :: Proxy CategoryStatus)
  , mkSumType (Proxy :: Proxy Item)
  , mkSumType (Proxy :: Proxy ItemKind)
  , mkSumType (Proxy :: Proxy Trait)
  ]

main :: IO ()
main = writePSTypes path (buildBridge bridge) clientTypes
