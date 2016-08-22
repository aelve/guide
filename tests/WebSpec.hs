{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module WebSpec (tests) where


import BasePrelude hiding (catch, bracket)
-- Monads
import Control.Monad.IO.Class
-- Concurrency
import qualified SlaveThread as Slave
-- Text
import Data.Text.All (Text)
-- Files
import System.Directory
-- Testing
import Test.Hspec.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.Exceptions
import qualified Test.Hspec.Expectations as Hspec
-- URLs
import Network.URI
-- Exceptions
import Control.Monad.Catch

-- Site
import qualified Guide
import Config (Config(..))


-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

tests :: IO ()
tests = run $ do
  mainPageTests
  categoryTests
  markdownTests

mainPageTests :: Spec
mainPageTests = session "main page" $ using Firefox $ do
  openGuide "/"
  wd "is initially empty" $ do
    checkPresent "#categories"
    checkNotPresent "#categories > *"
  wd "has a google-token" $ do
    e <- select "meta[name=google-site-verification]"
    e `shouldHaveAttr` ("content", "some-google-token")
  wd "has a title" $ do
    e <- select "h1"
    e `shouldHaveText` "Aelve Guide: Haskell"
  wd "has a subtitle" $ do
    checkPresent ".subtitle"
  describe "the footer" $ do
    wd "is present" $ do
      checkPresent "#footer"
    wd "isn't overflowing" $ do
      setWindowSize (900, 500)  -- the footer is about 800px wide
      e <- select "#footer"
      (width, height) <- elemSize e
      width  `shouldSatisfy` ("be <850", (<850))
      height `shouldSatisfy` ("be <70", (<70))
      -- and now it shall be overflowing
      setWindowSize (700, 500)
      (_, height2) <- elemSize e
      waitUntil 2 $
        height2 `shouldSatisfy` ("be >70", (>70))

categoryTests :: Spec
categoryTests = session "categories" $ using Firefox $ do
  openGuide "/"
  wd "add a new category" $ do
    createCategory "Some category"
    checkPresent ".category"
    url <- getCurrentRelativeURL
    uriPath url `shouldSatisfy`
      ("start with /haskell/some-category-",
       isPrefixOf "/haskell/some-category-")
  describe "created category" $ do
    wd "has a link to the main page" $ do
      e <- select "h1 > a"
      e `shouldHaveText` "Aelve Guide: Haskell"
      getBackAfterwards $ do
        changesURL $ click e
        url <- getCurrentRelativeURL
        uriPath url `shouldBe` "/haskell"
    wd "has a subtitle" $ do
      checkPresent ".subtitle"
    wd "doesn't have an add-category field" $ do
      checkNotPresent ".add-category"
  -- TODO: test that the feed button is present and that the feed is
  -- generated properly
  describe "category properties" $ do
    describe "title" $ do
      wd "is present" $ do
        e <- select categoryTitle
        e `shouldHaveText` "Some category"
      wd "can be changed" $ do
        form <- openCategoryEditForm
        do inp <- select (form, "input[name=title]" :: String)
           clearInput inp
           sendKeys ("Another category" <> _enter) inp
           waitWhile 2 (expectNotStale inp)
        e <- select categoryTitle
        e `shouldHaveText` "Another category"
    describe "group" $ do
      wd "is present" $ do
        e <- select ".category .group"
        e `shouldHaveText` "Miscellaneous"
      wd "can be changed" $ do
        form <- openCategoryEditForm
        do inp <- select (form, "input[name=group]" :: String)
           clearInput inp
           sendKeys ("Basics" <> _enter) inp
           waitWhile 2 (expectNotStale inp)
        e <- select ".category .group"
        e `shouldHaveText` "Basics"

markdownTests :: Spec
markdownTests = session "markdown" $ using Firefox $ do
  openGuide "/"
  describe "Markdown isn't allowed in category names" $ do
    wd "when creating a category" $ do
      createCategory "*foo*"
      e <- select categoryTitle
      e `shouldHaveText` "*foo*"
    wd "when changing existing category's name" $ do
      form <- openCategoryEditForm
      do inp <- select (form, "input[name=title]" :: String)
         clearInput inp
         sendKeys ("foo `bar`" <> _enter) inp
         waitWhile 2 (expectNotStale inp)
      e <- select categoryTitle
      e `shouldHaveText` "foo `bar`"

-----------------------------------------------------------------------------
-- Helpers dealing with guide specifically
-----------------------------------------------------------------------------

openGuide :: String -> SpecWith (WdTestSession ())
openGuide s = specify ("load " ++ s) $ runWD $
  openPage ("http://localhost:8080/haskell" ++ s)

-- Assumes that the main page is open
createCategory :: Text -> WD ()
createCategory t =
  changesURL $ sendKeys (t <> _enter) =<< select ".add-category"

categoryTitle :: Selector
categoryTitle = ByCSS ".category-title"

openCategoryEditForm :: WD Element
openCategoryEditForm = do
  click =<< select (".category h2", ByLinkText "edit")
  select ".category form"

categoryEditForm :: Selector
categoryEditForm = ByCSS ".category form"

-----------------------------------------------------------------------------
-- Utilities for webdriver
-----------------------------------------------------------------------------

class Show a => IsSelector a where
  selectAll :: a -> WD [Element]
instance IsSelector Element where
  selectAll e = return [e]
instance IsSelector Selector where
  selectAll s = findElems s
instance (IsSelector a) => IsSelector (a, Selector) where
  selectAll (a, b) = do
    es <- selectAll a
    nub . concat <$> for es (\e -> findElemsFrom e b)
instance (IsSelector a) => IsSelector (a, String) where
  selectAll (a, b) = do
    es <- selectAll a
    nub . concat <$> for es (\e -> findElemsFrom e (ByCSS (fromString b)))
instance (a ~ String) => IsSelector a where
  selectAll t = findElems (ByCSS (fromString t))

select :: IsSelector a => a -> WD Element
select x = do
  es <- selectAll x
  when (null es) $ expectationFailure $
    printf "%s wasn't found on the page" (show x)
  return (head es)

selectWait :: IsSelector a => a -> WD Element
selectWait s = waitUntil 2 (select s)
  `catch` \e@(FailedCommand ty _) ->
     if ty == Timeout
       then error (printf "Waiting for %s timed out" (show s))
       else throwM e

changesURL :: WD a -> WD a
changesURL x = do
  url <- getCurrentURL
  a <- x
  waitUntil 2 ((/= url) <$> getCurrentURL)
  return a

getBackAfterwards :: WD a -> WD a
getBackAfterwards x = do
  url <- getCurrentURL
  a <- x
  openPage url
  return a

_TODO :: MonadIO m => m ()
_TODO = error "test not implemented"

wd :: String -> WD a -> SpecWith (WdTestSession ())
wd x act = it x (runWD (void act))

_pause :: WD ()
_pause = do
  liftIO $ putStr "press Enter to continue testing, or “q” to quit: "
  x <- liftIO $ getLine
  when (x == "q") $
    expectationFailure "quit"

checkPresent :: IsSelector a => a -> WD ()
checkPresent x = do
  es <- selectAll x
  when (null es) $ expectationFailure $
    printf "expected %s to be present on the page" (show x)

checkNotPresent :: IsSelector a => a -> WD ()
checkNotPresent x = do
  es <- selectAll x
  when (not (null es)) $ expectationFailure $
    printf "expected %s not to be present on the page" (show x)

getCurrentRelativeURL :: WD URI
getCurrentRelativeURL = do
  url <- getCurrentURL
  case parseURI url of
    Nothing -> error ("couldn't parse as URL: " ++ url)
    Just u  -> do
      maybe "" uriRegName (uriAuthority u) `shouldBe` "localhost"
      return u

run :: Spec -> IO ()
run ts = do
  let prepare = do
        exold <- doesDirectoryExist "state-old"
        when exold $ error "state-old exists"
        ex <- doesDirectoryExist "state"
        when ex $ renameDirectory "state" "state-old"
        -- Start the server
        --
        -- Using 'Slave.fork' in 'Guide.mainWith' ensures that threads started
        -- inside of 'mainWith' will be killed too when the thread dies.
        tid <- Slave.fork $ Guide.mainWith Config {
          _baseUrl       = "/",
          _googleToken   = "some-google-token",
          _adminPassword = "123",
          _prerender     = False }
        -- Using a delay so that “Spock is running on port 8080” would be
        -- printed before the first test.
        threadDelay 100000
        return tid
  let finalise tid = do
        killThread tid
        ex <- doesDirectoryExist "state"
        when ex $ removeDirectoryRecursive "state"
        exold <- doesDirectoryExist "state-old"
        when exold $ renameDirectory "state-old" "state"
  bracket prepare finalise $ \_ -> do
    hspec ts

expectationFailure :: MonadIO m => String -> m ()
expectationFailure = liftIO . Hspec.expectationFailure

shouldSatisfy :: (Show a, MonadIO m) => a -> (String, a -> Bool) -> m ()
shouldSatisfy a (s, p) = unless (p a) $
  expectationFailure (printf "expected %s to %s" (show a) s)

_backspace, _enter, _esc :: Text
(_backspace, _enter, _esc) = ("\xE003", "\xE007", "\xE00C")
_shift, _ctrl, _alt, _command :: Text
(_shift, _ctrl, _alt, _command) = ("\xE008", "\xE009", "\xE00A", "\xE03D")

{-
NULL            \uE000
CANCEL          \uE001
HELP            \uE002
TAB             \uE004
CLEAR           \uE005
RETURN          \uE006
PAUSE           \uE00B
SPACE           \uE00D
PAGE_UP         \uE00E
PAGE_DOWN       \uE00F
END             \uE010
HOME            \uE011
ARROW_LEFT      \uE012
ARROW_UP        \uE013
ARROW_RIGHT     \uE014
ARROW_DOWN      \uE015
INSERT          \uE016
DELETE          \uE017
F1              \uE031
F2              \uE032
F3              \uE033
F4              \uE034
F5              \uE035
F6              \uE036
F7              \uE037
F8              \uE038
F9              \uE039
F10             \uE03A
F11             \uE03B
F12             \uE03C
META            \uE03D
ZENKAKU_HANKAKU \uE040

SEMICOLON       \uE018
EQUALS          \uE019
NUMPAD0         \uE01A
NUMPAD1         \uE01B
NUMPAD2         \uE01C
NUMPAD3         \uE01D
NUMPAD4         \uE01E
NUMPAD5         \uE01F
NUMPAD6         \uE020
NUMPAD7         \uE021
NUMPAD8         \uE022
NUMPAD9         \uE023
MULTIPLY        \uE024
ADD             \uE025
SEPARATOR       \uE026
SUBTRACT        \uE027
DECIMAL         \uE028
DIVIDE          \uE029
-}
