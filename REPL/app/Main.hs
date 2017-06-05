{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Int(Int64)
import qualified Control.Exception as X
import qualified Data.Char as DC
import qualified Data.List as DL
import Control.Monad(forever)
import System.Directory(copyFile)
import System.IO (stdout, hFlush)
import qualified Data.Map.Strict as Map

import REPL

defaultPBI :: ProcessBuilderInfo 
defaultPBI = PBI {
  archiveURL = "https://hackage.haskell.org/01-index.tar.gz",
  snapshotURL = "https://hackage.haskell.org/snapshot.json",
  archive = "01-index.tar.gz",
  archiveClone = "01-index.tar.gz.orig",
  tar = "01-index.tar",
  tarClone = "01-index.orig.tar" }

main :: IO ()
main = processCycle defaultPBI
