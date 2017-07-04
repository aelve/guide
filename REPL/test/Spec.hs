{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Either (isRight, either)
import qualified Data.Text as T
import Data.Monoid ((<>))
import System.FilePath((</>))
import HackageArchive
import Common

import qualified Data.Text.IO as TIO

testWorkingDir :: String
testWorkingDir = "testworkdir"

parseTests = testGroup "Hackage archive parsing tests"
  [
    testPath "filecollection/0.1.1.9/filecollection.cabal" "filecollection" True
    , testPath "filecollection/0.1.1.9/filecollection.cabal" "filecollectionz" False
    , testPath "file-collection/0.1.1.9/file-collection.cabal" "file-collection" True
    , testPath "file-collection/0.1.1.9/file-collection.cabal" "filecollection" False
  ]

testPath :: T.Text -> T.Text -> Bool -> TestTree
testPath text val match = testCase (T.unpack ("Parsing " <> expect match <> " \'" <> text <> "\'")) $
  assertBool "Failed" $ ((fst <$> parsePath (T.unpack text)) == Just (T.unpack val)) == match

{-
parseStackageTests = testGroup "Stackage parsing tests"
  [
    testParse parsePackageLine "              ztail ==1.2" True
    , testParse parsePackageLine "             adjunctions ==4.3," True
    , testParse parsePackageLine "ztail ==1.2" True
    , testParse parsePackageLine "adjunctions ==4.3," True

    , testParse parseLTS "-- Stackage snapshot from: http://www.stackage.org/snapshot/lts-2.10" True
    , testParse parseLTS "-- Stackage snapshot from: http://www.stackage.org/snapshot/lts-2.$10" True
    , testParse parseLTS "-- Please place this file next to your .cabal file as cabal.config" False
    , testParse parseLTS "-- To only use tested packages, uncomment the following line:" False
    , testParse parseLTS "-- remote-repo: stackage-lts-2.10:http://www.stackage.org/lts-2.10" False
    , testParse parseLTS "constraints: abstract-deque ==0.3," False
    , testParse parseLTS "abstract-par ==0.3.3," False
    , testParse parseLTS "zlib-lens ==0.1.2" False
    , testParse parseLTS "-- Stackage snapshot from: http://www.stackage.org/snapshot/nightly-2017-06-10" True
  ]

parseCabalConfig = testGroup "Cabal config parsing tests"
  [
    testStackagePackageLines parseStackageLTS "sometestfile.cnf"
    , testStackagePackageLines parseStackageLTS "sometestfile2.cnf"
    , testFileJustParse parseStackageLTS "sometestfile3.cnf" True
  ]
-}

-- Well this is code duplication. Somehow need to use testParse function here

testFileJustParse :: Parser a -> FilePath -> Bool -> TestTree
testFileJustParse p file match = testCase ("Testing file: " ++ file) $ do 
  fileText <- TIO.readFile (testWorkingDir </> file)
  assertBool "Failed" (isRight (runParser p "" fileText) == match)

testStackagePackageLines :: Parser StackageLTS -> FilePath -> TestTree
testStackagePackageLines p file = testFileParse (testWorkingDir </> file) 
  p countPackageLines matchWithStackageLTS

countPackageLines :: T.Text -> Int
countPackageLines text = length $ filter isPackageLine lns
  where lns = T.lines text
        isPackageLine ln = not ("--" `T.isInfixOf` ln) 
          && (("installed" `T.isInfixOf` ln) || ("==" `T.isInfixOf` ln))

matchWithStackageLTS :: Int -> StackageLTS -> Bool
matchWithStackageLTS count1 stackage = count1 == (length.snd) stackage

expect :: Bool -> T.Text
expect True = "expect success"
expect False = "expect fail"

testParse :: Parser a -> T.Text -> Bool -> TestTree
testParse p text match = testCase (T.unpack ("Parsing " <> expect match <> " \'" <> text <> "\'")) $
  assertBool "Failed" (isRight (runParser p "" text) == match)

testFileParse :: FilePath -> Parser a -> (T.Text -> b) -> (b -> a -> Bool) -> TestTree
testFileParse file p textFunc matchFunc = 
  testCase ("Testing file: " ++ file) $ do
    fileText <- TIO.readFile file -- got the file
    let textVal = textFunc fileText
    let eVal = matchFunc textVal <$> runParser p "" fileText
    assertBool "Failed" (either (const False) id eVal)

tests :: TestTree
tests = testGroup "REPL tests" [parseTests]

main = defaultMain tests
