{-# LANGUAGE OverloadedStrings #-}

module Stackage(
                parseLTS, 
                parsePackageLine,
                parseStackageLTS,
                StackageLTS) where

import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Text  
import qualified Text.Megaparsec.Lexer as L

import qualified Data.Version as DV
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad (void)
import Control.Applicative(empty)

import Common

type ConstraintMap = M.Map PackageName PackageData

parseStackageLTS :: Parser StackageLTS
parseStackageLTS = do
  ltsName <- parseLTS
  eol
  manyTill anyChar (string "constraints:")
  packages <- many parsePackageLine
  pure (ltsName, packages)

parseLTS :: Parser LongSnapshotName
parseLTS = do
  manyTill anyChar (string "http://www.stackage.org/snapshot/")
  name <- some (letterChar <|> digitChar <|> char '.' <|> char '-')
  pure name

parsePackageLine :: Parser PackageData
parsePackageLine = do
  space
  name <- some (letterChar <|> digitChar <|> char '-')
  space
  version <- parseVersionVer
  many (char ',') 
  space
  pure (name, Specified version)

-- unfortunately the cabal.config does not provide versions for several packages
-- And writes tehn in form 'binary installed'
-- Don't know what to do with this situation now
parseVersionVer :: Parser DV.Version
parseVersionVer = (string "==" >> parseVersion) <|> (string "installed" >> return (DV.Version [6,6,6] []))

parseVersion :: Parser DV.Version
parseVersion = do 
  numbers <- sepBy1 L.integer (char '.')
  pure $ DV.Version (map fromIntegral numbers) []
