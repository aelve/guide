{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module StackageArchive where

import qualified Data.Map.Strict as M
import qualified Data.Version as DV

import Common
{-
type StackageName = String
type StackageVersionLTS = M.Map LongSnapshotName DV.Version

data StackagePackage = SP {
  name :: StackageName,
  ltsVersions :: StackageVersionLTS
} deriving (Eq, Show)

type StackageMap = M.Map StackageName StackagePackage
-}