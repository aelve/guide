{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Either (isRight, either)
import qualified Data.Text as T
import Data.Monoid ((<>))
import HackageArchive
import Stackage

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

parseStackageTests = testGroup "Stackage parsing tests"
  [
    testParse parsePackageLine "constraints: abstract-deque ==0.3," True
    , testParse parsePackageLine "constraints: abstract-deque ==0.3" True
    , testParse parsePackageLine "constraints: abstract-deque ==0." False
    , testParse parsePackageLine "constraints: abstract-deque ==" False
    , testParse parsePackageLine "constraints: abst3453#$%#ract-deque ==0.3" False
    , testParse parsePackageLine "constraints: abstract-deque ==0.3," True
    , testParse parsePackageLine "              ztail ==1.2" True
    , testParse parsePackageLine "             adjunctions ==4.3," True
    , testParse parsePackageLine "ztail ==1.2" True
    , testParse parsePackageLine "adjunctions ==4.3," True

    , testParse parseLTSLine "-- Stackage snapshot from: http://www.stackage.org/snapshot/lts-2.10" True
    , testParse parseLTSLine "-- Stackage snapshot from: http://www.stackage.org/snapshot/lts-2.$10" False
    , testParse parseLTSLine "-- Please place this file next to your .cabal file as cabal.config" False
    , testParse parseLTSLine "-- To only use tested packages, uncomment the following line:" False
    , testParse parseLTSLine "-- remote-repo: stackage-lts-2.10:http://www.stackage.org/lts-2.10" False
    , testParse parseLTSLine "constraints: abstract-deque ==0.3," False
    , testParse parseLTSLine "abstract-par ==0.3.3," False
    , testParse parseLTSLine "zlib-lens ==0.1.2" False
    , testParse parseLTSLine "-- Stackage snapshot from: http://www.stackage.org/snapshot/nightly-2017-06-10" True
  ]
{-
parseCabalConfig = (testWorkDir </> "sometestfile.cnf") testGroup "Cabal config parser tests"
  [
    testFileParse parseStackageLTS
  ]
-}

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
tests = testGroup "REPL tests" [parseStackageTests, parseTests]

main = defaultMain tests
