{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for logging of errors.
module LogSpec (tests) where

import BasePrelude
import System.IO
import Text.RE.TDFA.String

import Test.Hspec


getLines :: Handle -> IO String
getLines h = loop' []
  where
    loop' :: [String] -> IO String
    loop' xs = do
      eLine <- try $ hGetLine h
      case eLine of
        Left (_ :: SomeException) -> pure $ concat $ reverse xs
        Right line                -> loop' (line:xs)

tests :: FilePath -> Spec
tests logFile = describe "test of logger" $ do
  logs <- runIO $ do
    logFileHandle <- openFile logFile ReadWriteMode
    logs <- getLines logFileHandle
    hClose logFileHandle
    pure logs

  describe "Logging of init" $ do
    it "Spock init message is present" $ [re|Spock is running on port|] `isIn` logs
    it "Api init message is present" $ [re|API is running on port|] `isIn` logs
  describe "Logging of api" $ do
    describe "Categories" $ do
      it "modify notes to category request" $ [re|setCategoryNotes|] `isIn` logs
    describe "Item" $ do
      it "set item info" $ [re|setItemInfo|] `isIn` logs
    describe "Trait" $ do
      it "move trait" $ [re|moveTrait|] `isIn` logs

    describe "Errors (exceptions)" $ do
      it "Category not found" $
        [re|response code 404: Category not found|] `isIn` logs

isIn :: HasCallStack => RE -> String -> Expectation
isIn reg text = case matchedText $ text ?=~ reg of
  Just _ -> pure ()
  Nothing -> expectationFailure text
