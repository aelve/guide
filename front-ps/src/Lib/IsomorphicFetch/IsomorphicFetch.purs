module Lib.IsomorphicFetch where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Data.Function.Uncurried (Fn3, runFn3)


foreign import data FETCH :: Effect

type URI = String

foreign import fetchImpl :: forall a eff . Fn3 URI
  (a -> Eff (fetch :: FETCH | eff) Unit)
  (Error -> Eff (fetch :: FETCH | eff) Unit)
  (Eff (fetch :: FETCH | eff) Unit)


fetch :: forall a eff. URI -> Aff (fetch :: FETCH | eff) a
fetch uri = makeAff \errCb successCb ->
  runFn3 fetchImpl uri successCb errCb
