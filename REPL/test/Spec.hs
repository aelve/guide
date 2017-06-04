import Test.Tasty
import Test.Tasty.HUnit
import Data.Ord

import TarUtil as TU

parseTests = testGroup "Different parsing tests"
  [
--    testCase "'-' chat in package name parsing" $
--    (fst <$> parsePath "file-collection/0.1.1.9/file-collection.cabal") 
--      `compare` (Just "file-collection") @?= EQ,

    testCase "Package name parsing" $
    (fst <$> parsePath "filecollection/0.1.1.9/filecollection.cabal") == (Just "filecollection") @?= True
    
    , testCase "Package name parsing" $
    (fst <$> parsePath "filecollection/0.1.1.9/filecollection.cabal") == (Just "filecollectionz") @?= False
    
    , testCase "Package name parsing" $
    (fst <$> parsePath "file-collection/0.1.1.9/file-collection.cabal") == (Just "file-collection") @?= True

    , testCase "Package name parsing" $
    (fst <$> parsePath "file-collection/0.1.1.9/file-collection.cabal") == (Just "filecollection") @?= False
  ]
{-
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
-}

tests :: TestTree
tests = testGroup "REPL tests" [parseTests]


main = defaultMain parseTests
