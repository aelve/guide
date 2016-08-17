{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
(
  main,
)
where


import BasePrelude hiding (catch, bracket)
-- Monads
import Control.Monad.IO.Class
-- Concurrency
import qualified SlaveThread as Slave
-- Text
import Data.Text (Text)
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

main :: IO ()
main = run $ do
  mainPageTests
  categoryTests
  -- TODO: noscript tests
  -- TODO: test on mobile
  -- TODO: test that there are no repetitive searches on the admin page
  -- TODO: test that admin CSS hasn't creeped into main CSS and vice-versa

mainPageTests :: Spec
mainPageTests = session "main page" $ using Firefox $ do
  openGuide "/"
  wd "is initially empty" $ do
    void $ select "#categories"
    es <- selectAll "#categories > *"
    length es `shouldBe` 0
  wd "has a google-token" $ do
    e <- select "meta[name=google-site-verification]"
    e `shouldHaveAttr` ("content", "some-google-token")
  wd "has a title" $ do
    e <- select "h1"
    e `shouldHaveText` "Aelve Guide: Haskell"
  wd "has a subtitle" $ do
    select ".subtitle"
  describe "the footer" $ do
    wd "is present" $ do
      select "#footer"
    wd "isn't overflowing" $ do
      setWindowSize (900, 500)  -- the footer is about 800px wide
      e <- select "#footer"
      (width, height) <- elemSize e
      width  `shouldSatisfy` ("be <850", (<850))
      height `shouldSatisfy` ("be <70", (<70))
      -- and now it shall be overflowing
      setWindowSize (700, 500)
      (_, height2) <- elemSize e
      height2 `shouldSatisfy` ("be >70", (>70))

categoryTests :: Spec
categoryTests = session "categories" $ using Firefox $ do
  openGuide "/"
  wd "add a new category" $ do
    sendKeys ("Some category" <> _enter) =<< select ".add-category"
    selectWait ".category"
    url <- getCurrentRelativeURL
    uriPath url `shouldSatisfy`
      ("start with /haskell/some-category-",
       isPrefixOf "/haskell/some-category-")
  describe "created category" $ do
    wd "has a link to the main page" $ do
      e <- select "h1 > a"
      e `shouldHaveText` "Aelve Guide: Haskell"
      click e
      url <- getCurrentRelativeURL
      uriPath url `shouldBe` "/haskell"
      back
      selectWait ".category"
    wd "has a subtitle" $ do
      select ".subtitle"
    wd "doesn't have an add-category field" $ do
      es <- selectAll ".add-category"
      es `shouldBe` []

-- markdown tests
-- • markdown doesn't work in category names

-----------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------

_TODO :: MonadIO m => m ()
_TODO = error "test not implemented"

wd :: String -> WD a -> SpecWith (WdTestSession ())
wd x act = it x (runWD (void act))

_pause :: MonadIO m => m ()
_pause = liftIO $ void $ do
  putStr "press Enter to continue testing: "
  getLine

select :: Text -> WD Element
select = findElem . ByCSS

selectAll :: Text -> WD [Element]
selectAll = findElems . ByCSS

selectWait :: Text -> WD Element
selectWait css = waitUntil 2 (select css)
  `catch` \e@(FailedCommand ty _) ->
     if ty == Timeout
       then error (printf "Waiting for “%s” timed out" css)
       else throwM e

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

openGuide :: String -> SpecWith (WdTestSession ())
openGuide s = specify ("load " ++ s) $ runWD $
  openPage ("http://localhost:8080/haskell" ++ s)

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
