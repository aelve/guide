{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (main) where


import BasePrelude
-- Testing
import Test.Hspec

-- Tests
import qualified WebSpec
import qualified MarkdownSpec


main :: IO ()
main = do
  hspec $ do
    MarkdownSpec.tests
  -- TODO: it'd be nice if we could us WebSpec.tests in hspec as well,
  -- but I don't know how to achieve the following:
  --   * before WebSpec tests, the server is started
  --   * after those tests, the server is killed
  --   * if you Ctrl-C during the tests, the server is killed as well
  WebSpec.tests

{- TODO
  * noscript tests
  * test on mobile
  * test that there are no repetitive searches on the admin page
  * test that admin CSS hasn't creeped into main CSS and vice-versa
  * test that the server is throwing no errors whatsoever during the
    execution of tests
  * changes to item description must not persist when doing Cancel and
    then Edit again
  * test that pages are indeed cached
  * test that changing some pages doesn't invalidate the entire cache
  * Markdown tests (e.g. Markdown doesn't work in category names)
  * test that nothing is messed up by things starting and ending with newlines
    (the %js bug, see description of 'mustache')
  -}
