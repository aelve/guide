{-# LANGUAGE OverloadedStrings #-}

module Stackage(
                parseLTSLine, 
                parsePackageLine) where

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
type ShortSnapshotName = String
type LongSnapshotName = String
type StackageSnapshot = (ShortSnapshotName, LongSnapshotName)

shortName :: StackageSnapshot -> String
shortName = fst

longName :: StackageSnapshot -> String
longName = snd

type StackageLTS = (LongSnapshotName, [PackageData])

parseStackageLTS :: Parser StackageLTS
parseStackageLTS = do
  many (try (manyTill anyChar eol >> notFollowedBy parseLTSLine))
  ltsName <- parseLTSLine
  --packages = 
  pure (ltsName, [])

parseLTSLine :: Parser LongSnapshotName
parseLTSLine = do
  -- destroy everything 
  manyTill anyChar (string "http://www.stackage.org/snapshot/")
  name <- some (letterChar <|> digitChar <|> char '.' <|> char '-')
  space
  void eol <|> eof 
  pure name

parsePackageLine :: Parser PackageData
parsePackageLine = do
  packageData <- try parsePackageConst <|> parsePackageEmpty
  space 
  manyTill anyChar (void eol <|> eof)
  pure packageData

parsePackageConst :: Parser PackageData
parsePackageConst = do
  manyTill anyChar (char ':') -- chop the 'constraints:' in the beginning
  parsePackageEmpty
 
parsePackageEmpty :: Parser PackageData
parsePackageEmpty = do
  space  
  parsePackageData

parsePackageData :: Parser PackageData
parsePackageData = do
  name <- some (letterChar <|> digitChar <|> char '-')
  space
  string "=="
  space
  version <- parseVersion 
  many (char ',')
  pure (name, version)

parseVersion :: Parser DV.Version
parseVersion = do 
  numbers <- sepBy1 L.integer (char '.')
  pure $ DV.Version (map fromIntegral numbers) []
