{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default
import REPL
import qualified AllCommands as AC
import PackageManager

main :: IO ()
-- launches package manager update, that performs update every 60 minutes
--main = launchPackageUpdater def 60 
-- launches REPL for hackage and stackage
main = processREPLCycle def AC.allCommands
