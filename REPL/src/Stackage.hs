module Stackage() where

import qualified Data.Version as DV
import qualified Data.Map as M

import Common

type ConstraintMap = M.Map PackageName PackageData

-- the minor and major versions for stackage
type StackageLTS = (Int, Int)

data LTS = LTS {
  name :: String,
  url :: URL,
  constraints :: ConstraintMap 
}



