{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module MergeSpec (tests) where


import BasePrelude
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Testing
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Text.Arbitrary ()

import Guide.Diff.Merge


tests :: Spec
tests = describe "merging" $ do
  -- <from>: <variant A>+<variant B> = <merged>
  describe "simple cases" $ do
    describe "A: A+A = A" $
      prop "QuickCheck" $ \(M a) ->
        merge a a a === a
    describe "A: B+B = B" $
      prop "QuickCheck" $ \(M a) (M b) ->
        merge a b b === b
    describe "A: A+B = B" $
      prop "QuickCheck" $ \(M a) (M b) ->
        merge a a b === b

  describe "replacing" $ do
    it "different words" $ do
      merge "a b c" "x b c" "a b y" `shouldBe` "x b y"
    it "words separated by /s" $ do
      merge "a/b/c" "x/b/c" "a/b/y" `shouldBe` "x/b/y"
    it "punctuation" $ do
      merge "a,b,c" "a,b!c" "a?b,c" `shouldBe` "a?b!c"
    it "words with similar words" $ do
      merge "foobar" "foobir" "xoobar" `shouldBe` "foobir"

  describe "inserting" $ do
    it "words in different but near places" $ do
      merge "f x" "A B C f x" "f d b e x" `shouldBe` "A B C f d b e x"
    it "words from different sides" $ do
      merge "x" "foo x" "x bar" `shouldBe` "foo x bar"
      merge "x" "a foo   x" "x bar" `shouldBe` "a foo   x bar"
    it "words and punctuation in random places" $ do
      merge "a b,c d" " a x/b, c d" "a b,c d e" `shouldBe` " a x/b, c d e"
    it "several words" $ do
      merge "a b c d" "a a a b c d e" "a b c e d" `shouldBe` "a a a b c e d e"

  describe "deleting" $ do
    it "words" $ do
      merge "a B c D" "a c D" "a B c" `shouldBe` "a c"

  describe "tokens" $ do
    it "links aren't merged" $ do
      merge "http://a/b/c" "http://x/b/c" "http://a/b/y"
        `shouldBe` "http://x/b/c"

newtype M = M Text
  deriving (Eq, Ord)

instance Show M where
  show (M x) = show x

instance Arbitrary M where
  shrink (M x) = map M (shrink x)
  arbitrary = M <$> oneof [
    return "",
    T.singleton <$> elements "abc123",
    T.singleton <$> elements " .,()*+/",
    arbitrary ]
