{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default
import IndexProject

main :: IO ()
main = processREPLCycle def
