{-# LANGUAGE DataKinds #-}

module Main
  ( main
  ) where

import Prelude
import Data.Proxy (Proxy (..))
import Language.PureScript.Bridge (BridgeBuilder, Language( Haskell ), SumType,
                                    PSType, TypeInfo (..), (<|>), (^==), buildBridge,
                                    defaultBridge, typeName, mkSumType, writePSTypes)
import Guide.Api.ClientTypes (CCategoryDetail, CGrandCategory)

path :: FilePath
path = "front-ps/src/Generated"

psPosixTime :: PSType
psPosixTime = TypeInfo "" "Data.Time.NominalDiffTime" "NominalDiffTime" []

posixTimeBridge :: BridgeBuilder PSType
posixTimeBridge =
  typeName ^== "NominalDiffTime" >> pure psPosixTime

bridge :: BridgeBuilder PSType
bridge = defaultBridge
  <|> posixTimeBridge

clientTypes :: [SumType  'Haskell]
clientTypes =
  [ mkSumType (Proxy :: Proxy CGrandCategory)
  , mkSumType (Proxy :: Proxy CCategoryDetail)
  ]

main :: IO ()
main = writePSTypes path (buildBridge bridge) clientTypes
