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
import Data.Default

import IndexProject

main :: IO ()
main = processCycle def
