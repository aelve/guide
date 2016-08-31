{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module WebSpec (tests) where


import BasePrelude hiding (catch, bracket)
-- Monads
import Control.Monad.IO.Class
import Control.Monad.Loops
-- Concurrency
import qualified SlaveThread as Slave
-- Containers
import qualified Data.Set as Set
-- Text
import Data.Text.All (Text)
import qualified Data.Text.All as T
-- Files
import System.Directory
-- Testing
import Test.Hspec.WebDriver hiding (shouldHaveAttr, shouldHaveText, click)
import qualified Test.Hspec.WebDriver as WD
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
import Utils (ordNub)


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
    "meta[name=google-site-verification]" `shouldHaveAttr`
      ("content", "some-google-token")
  wd "has a title" $ do
    "h1" `shouldHaveText` "Aelve Guide: Haskell"
  describe "subtitle" $ do
    wd "is present" $ do
      sub <- select ".subtitle"
      fs <- fontSize sub; fs `shouldBeInRange` (15,17)
    wd "has a discuss link" $ do
      checkPresent ".subtitle a[href='http://discuss.link']"
  describe "footer" $ do
    wd "is present" $ do
      checkPresent "#footer"
    wd "isn't overflowing" $ do
      setWindowSize (900, 500)  -- the footer is about 800px wide
      footer <- select "#footer"
      (width, height) <- elemSize footer
      width `shouldBeInRange` (750, 850)
      height `shouldBeInRange` (60, 70)
      -- and now it shall be overflowing
      setWindowSize (700, 500)
      waitUntil 2 (expect . inRange (90, 140) . snd =<< elemSize footer)
        `catch` \(_::ExpectFailed) -> return ()
      height2 <- snd <$> elemSize footer
      height2 `shouldBeInRange` (90, 140)

categoryTests :: Spec
categoryTests = session "categories" $ using Firefox $ do
  openGuide "/"
  wd "add a new category" $ do
    createCategory "Some category"
    checkPresent ".category"
    url <- getCurrentRelativeURL
    (slug, _) <- parseCategoryURL (uriPath url)
    slug `shouldBe` "some-category"
  describe "created category" $ do
    wd "has a link to the main page" $ do
      titleLink <- select "h1 > a"
      titleLink `shouldHaveText` "Aelve Guide: Haskell"
      getBackAfterwards $ do
        changesURL $ click titleLink
        url <- getCurrentRelativeURL
        uriPath url `shouldBe` "/haskell"
    wd "has a subtitle" $ do
      checkPresent ".subtitle"
    wd "doesn't have an add-category field" $ do
      checkNotPresent ".add-category"
    wd "is present on the main page" $ do
      catURL <- getCurrentURL
      openGuidePage "/"
      changesURL $ click (ByLinkText "Some category")
      do u <- getCurrentURL
         u `shouldBe` catURL
    wd "is initially empty" $ do
      checkPresent ".items"
      checkNotPresent (".items" :// Not ".dummy")
  describe "category properties" $ do
    describe "title" $ do
      wd "is present" $ do
        e <- select categoryTitle; e `shouldHaveText` "Some category"
        fs <- fontSize e; fs `shouldBeInRange` (20, 26)
      wd "can be changed" $ do
        form <- openCategoryEditForm
        do inp <- select (form :// "input[name=title]")
           clearInput inp
           sendKeys ("Cat 1" <> _enter) inp
           checkNotPresent (form :& Displayed)
        categoryTitle `shouldHaveText` "Cat 1"
      wd "changes page slug when changed" $ do
        changesURL $ refresh
        do (slug, _) <- parseCategoryURL . uriPath =<< getCurrentRelativeURL
           slug `shouldBe` "cat-1"
        form <- openCategoryEditForm
        do inp <- select (form :// "input[name=title]")
           clearInput inp
           sendKeys ("Cat 2" <> _enter) inp
           checkNotPresent (form :& Displayed)
        changesURL $ refresh
        do (slug, _) <- parseCategoryURL . uriPath =<< getCurrentRelativeURL
           slug `shouldBe` "cat-2"
      wd "is changed on the front page too" $ do
        (_, id1) <- parseCategoryURL . uriPath =<< getCurrentRelativeURL
        openGuidePage "/"
        checkNotPresent (ByLinkText "Some category")
        checkNotPresent (ByLinkText "Cat 1")
        changesURL $ click (ByLinkText "Cat 2")
        (slug2, id2) <- parseCategoryURL . uriPath =<< getCurrentRelativeURL
        id1 `shouldBe` id2; slug2 `shouldBe` "cat-2"
    describe "group" $ do
      wd "is present" $ do
        group_ <- select categoryGroup
        group_ `shouldHaveText` "Miscellaneous"
        fs <- fontSize group_; fs `shouldBeInRange` (12, 15)
      wd "can be changed" $ do
        form <- openCategoryEditForm
        do inp <- select (form :// "input[name=group]")
           clearInput inp
           sendKeys ("Basics" <> _enter) inp
           checkNotPresent (form :& Displayed)
        categoryGroup `shouldHaveText` "Basics"
      wd "is changed on the front page too" $ do
        onAnotherPage "/" $ do
          catLink <- select (ByLinkText "Cat 2")
          groupHeader <- select ((".category-group" :<// catLink) :// "h2")
          groupHeader `shouldHaveText` "Basics"
    describe "status" $ do
      wd "is “stub” by default" $ do
        form <- openCategoryEditForm
        chosenOption <- select (form :// "select[name=status] option:checked")
        chosenOption `shouldHaveText` "Stub"
        onAnotherPage "/" $ do
          ByLinkText "Cat 2" `shouldHaveAttr` ("class", "status-stub")
      wd "can be changed" $ do
        form <- openCategoryEditForm
        sel <- select (form :// "select[name=status]")
        opt <- select (sel :// HasText "Complete")
        selectDropdown sel opt
        click (form :// ".save")
        onAnotherPage "/" $ do
          ByLinkText "Cat 2" `shouldHaveAttr` ("class", "status-finished")
    wd "create two items for further tests" $ do
      createItem "some item"
      createItem "another item"
    describe "pros/cons enabled" $ do
      wd "checkbox enabled by default" $ do
        form <- openCategoryEditForm
        checkbox <- select (form :// "[name=pros-cons-enabled]")
        shouldBeSelected checkbox
        click (form :// ".cancel")
      wd "section is shown in an item" $ do
        mapM_ shouldBeDisplayed =<< selectAll ".item-traits"
      wd "section isn't shown after unchecking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// "[name=pros-cons-enabled]")
        click (form :// ".save")
        waitUntil 2 $
          expect . not =<< anyM isDisplayed =<< selectAll ".item-traits"
      wd "section is shown again after checking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// "[name=pros-cons-enabled]")
        click (form :// ".save")
        waitUntil 2 $
          expect =<< allM isDisplayed =<< selectAll ".item-traits"
    describe "ecosystem enabled" $ do
      wd "checkbox enabled by default" $ do
        form <- openCategoryEditForm
        checkbox <- select (form :// "[name=ecosystem-enabled]")
        shouldBeSelected checkbox
        click (form :// ".cancel")
      wd "section is shown in an item" $ do
        mapM_ shouldBeDisplayed =<< selectAll ".item-ecosystem"
      wd "section isn't shown after unchecking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// "[name=ecosystem-enabled]")
        click (form :// ".save")
        waitUntil 2 $
          expect . not =<< anyM isDisplayed =<< selectAll ".item-ecosystem"
      wd "section is shown again after checking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// "[name=ecosystem-enabled]")
        click (form :// ".save")
        waitUntil 2 $
          expect =<< allM isDisplayed =<< selectAll ".item-ecosystem"
    describe "deleting a category" $ do
      wd "dismissing the alert doesn't do anything" $ do
        click (".category h2" :// ByLinkText "delete")
        dismissAlert
        catURL <- getCurrentURL
        openGuidePage "/"
        changesURL $ click (ByLinkText "Cat 2")
        do u <- getCurrentURL
           u `shouldBe` catURL
      wd "accepting the alert deletes the category" $ do
        catURL <- getCurrentURL
        changesURL $ do
          click (".category h2" :// ByLinkText "delete")
          acceptAlert
        url <- getCurrentRelativeURL
        uriPath url `shouldBe` "/haskell"
        checkNotPresent (ByLinkText "Cat 2")
        openPage catURL
        "body" `shouldHaveText` "Something went wrong"
  -- Feed button works
  -- Description editing works

markdownTests :: Spec
markdownTests = session "markdown" $ using Firefox $ do
  openGuide "/"
  describe "Markdown isn't allowed in category names" $ do
    wd "when creating a category" $ do
      createCategory "*foo*"
      categoryTitle `shouldHaveText` "*foo*"
    wd "when changing existing category's name" $ do
      form <- openCategoryEditForm
      do inp <- select (form :// "input[name=title]")
         clearInput inp
         sendKeys ("foo `bar`" <> _enter) inp
         checkNotPresent (form :& Displayed)
      categoryTitle `shouldHaveText` "foo `bar`"

-----------------------------------------------------------------------------
-- Helpers dealing with guide specifically
-----------------------------------------------------------------------------

parseCategoryURL :: String -> WD (String, String)
parseCategoryURL url = do
  case T.stripPrefix "/haskell/" (T.toStrict url) of
    Nothing -> expectationFailure $
                 printf "%s doesn't start with /haskell/" (show url)
    Just u -> do
      let (slug, catId) = T.breakOnEnd "-" u
      slug `shouldSatisfy` ("not null", not . T.null)
      T.last slug `shouldBe` '-'
      return (T.toString (T.init slug), T.toString catId)

openGuide :: String -> SpecWith (WdTestSession ())
openGuide s = wd ("load " ++ s) (openGuidePage s)

openGuidePage :: String -> WD ()
openGuidePage s = changesURL $ openPage ("http://localhost:8080/haskell" ++ s)

onAnotherPage :: String -> WD a -> WD a
onAnotherPage s x = getBackAfterwards $ do
  openGuidePage s
  x

-- Assumes that the main page is open
createCategory :: Text -> WD ()
createCategory t =
  changesURL $ sendKeys (t <> _enter) =<< select ".add-category"

-- Assumes that the category page is open
createItem :: Text -> WD Element
createItem t = do
  let selectItems = selectAll ".item"
  items <- selectItems
  sendKeys (t <> _enter) =<< select ".add-item"
  waitUntil 2 (expect . (\xs -> length xs > length items) =<< selectItems)
  items2 <- selectItems
  case items2 \\ items of
    [] -> expectationFailure "an item wasn't created"
    [x] -> return x
    _ -> expectationFailure "more than one item was created"

categoryTitle :: Selector
categoryTitle = ByCSS ".category-title"

categoryGroup :: Selector
categoryGroup = ByCSS ".category .group"

openCategoryEditForm :: WD Element
openCategoryEditForm = do
  click (".category h2" :// ByLinkText "edit")
  select ".category-info form"

-----------------------------------------------------------------------------
-- Utilities for webdriver
-----------------------------------------------------------------------------

isAlive :: Element -> WD Bool
isAlive e = (isEnabled e >> return True) `onDead` return False

onDead :: WD a -> WD a -> WD a
onDead x h = do
  let handler ex@(FailedCommand t _)
        | t `elem` [NoSuchElement, StaleElementReference] = h
        | otherwise = throw ex
  x `catch` handler

tryDead :: WD a -> WD (Maybe a)
tryDead x = (Just <$> x) `onDead` return Nothing

-- TODO: can fail if the element becomes stale between 'select' and 'click'
click :: CanSelect a => a -> WD ()
click s = WD.click =<< select s

-- TODO: can fail if the element becomes stale between 'select' and
-- 'shouldHaveAttr'
shouldHaveAttr :: CanSelect a => a -> (Text, Text) -> WD ()
s `shouldHaveAttr` (a, txt) = do
  e <- select s
  e `WD.shouldHaveAttr` (a, txt)

-- TODO: can fail if the element becomes stale between 'select' and
-- 'shouldHaveText'
shouldHaveText :: CanSelect a => a -> Text -> WD ()
s `shouldHaveText` txt = do
  e <- select s
  e `WD.shouldHaveText` txt

highlight :: Element -> WD ()
highlight e = do
  html <- executeJS [JSArg e]
    "arguments[0].style.border='thick solid #FF0000';\
    \return arguments[0].outerHTML;"
  liftIO $ putStrLn html

selectDropdown
  :: Element   -- ^ Dropdown
  -> Element   -- ^ Option to select
  -> WD ()
selectDropdown sel opt = void
  (executeJS [JSArg sel, JSArg opt]
     "sel=arguments[0];opt=arguments[1];\
     \for (var i=0;i<sel.options.length;i++)\
     \{if (sel.options[i]==opt)\
     \{sel.selectedIndex=i;break;}}" :: WD (Maybe ()))

getDescendants :: Element -> WD [Element]
getDescendants e = findElemsFrom e (ByXPath ".//*")

getChildren :: Element -> WD [Element]
getChildren e = findElemsFrom e (ByXPath "./*")

data ComplexSelector where
  -- | Descendants (not including the element itself)
  (://) :: (CanSelect a, CanSelect b) => a -> b -> ComplexSelector
  -- | Children
  (:/) :: (CanSelect a, CanSelect b) => a -> b -> ComplexSelector
  -- | Parents
  (:<//) :: (CanSelect a, CanSelect b) => a -> b -> ComplexSelector
  -- | Direct parents
  (:</) :: (CanSelect a, CanSelect b) => a -> b -> ComplexSelector
  -- | And
  (:&) :: (CanSelect a, CanSelect b) => a -> b -> ComplexSelector
  -- | Or
  (:|) :: (CanSelect a, CanSelect b) => a -> b -> ComplexSelector
  -- | Not
  Not :: CanSelect a => a -> ComplexSelector
  -- | Elements with specific text
  HasText :: Text -> ComplexSelector
  -- | Elements that contain specific text
  ContainsText :: Text -> ComplexSelector
  -- | Only pick the first N selected elements
  Take :: CanSelect a => Int -> a -> ComplexSelector
  -- | Displayed element
  Displayed :: ComplexSelector

deriving instance Show ComplexSelector

defSelectAll :: CanSelect a => a -> WD [Element]
defSelectAll s = filterElems s =<< findElems (ByXPath "//")

defFilterElems :: CanSelect a => a -> [Element] -> WD [Element]
defFilterElems s es = do
  ss <- Set.fromList <$> selectAll s
  return (filter (`Set.member` ss) es)

defAnyElem :: CanSelect a => a -> [Element] -> WD Bool
defAnyElem s es = do
  ss <- Set.fromList <$> selectAll s
  return (any (`Set.member` ss) es)

{- NOTE [staleness]
~~~~~~~~~~~~~~~

We want to avoid “stale element” errors at all costs, instead preferring “element not found” and so on. This means that whenever we select an element with 'select' or 'selectAll' and then do something with it, we have to catch and handle possible staleness.
-}

class Show a => CanSelect a where
  selectAll :: a -> WD [Element]
  selectAll = defSelectAll
  filterElems :: a -> [Element] -> WD [Element]
  filterElems = defFilterElems
  anyElem :: a -> [Element] -> WD Bool
  anyElem = defAnyElem
instance CanSelect Element where
  selectAll e = filterM isAlive [e]
  filterElems s es = do
    alive <- isAlive s
    return $ if alive then filter (s ==) es else []
  anyElem s es = do
    alive <- isAlive s
    return $ if alive then any (s ==) es else False
instance CanSelect Selector where
  selectAll s = findElems s
instance (a ~ Text) => CanSelect a where
  selectAll t = findElems (ByCSS t)
instance CanSelect ComplexSelector where
  selectAll s = do
    let getDescendants' e = getDescendants e `onDead` return []
        getChildren' e = getChildren e `onDead` return []
    case s of
      (a :// b) -> do
        as <- selectAll a
        ordNub.concat <$> mapM (filterElems b <=< getDescendants') as
      (a :/ b) -> do
        as <- selectAll a
        ordNub.concat <$> mapM (filterElems b <=< getChildren') as
      (a :<// b) -> filterM (anyElem b <=< getDescendants') =<< selectAll a
      (a :</  b) -> filterM (anyElem b <=< getChildren') =<< selectAll a
      (a :& b) -> do
        filterElems b =<< selectAll a
      (a :| b) -> do
        as <- Set.fromList <$> selectAll a
        bs <- Set.fromList <$> selectAll b
        return (Set.toList (as `Set.union` bs))
      Take n a -> take n <$> selectAll a
      --
      Not a          -> defSelectAll (Not a)
      HasText      t -> defSelectAll (HasText t)
      ContainsText t -> defSelectAll (ContainsText t)
      Displayed      -> defSelectAll Displayed
  filterElems s es = do
    let andNotDead = fmap (== Just True) . tryDead
    case s of
      Not a -> (es \\) <$> filterElems a es
      HasText      t -> filterM (andNotDead . fmap (== t) . getText) es
      ContainsText t -> filterM (andNotDead .
                                 fmap (t `T.isInfixOf`) . getText) es
      Displayed -> filterM (andNotDead . isDisplayed) es
      _ -> defFilterElems s es
  anyElem s es = do
    let andNotDead = fmap (== Just True) . tryDead
    case s of
      Not a -> (== length es) . length <$> filterElems a es
      HasText      t -> anyM (andNotDead . fmap (== t) . getText) es
      ContainsText t -> anyM (andNotDead .
                              fmap (t `T.isInfixOf`) . getText) es
      Displayed -> anyM (andNotDead . isDisplayed) es
      _ -> defAnyElem s es  

class ToSelector a where
  toSelector :: a -> Selector
instance ToSelector Selector where
  toSelector = id
instance ToSelector Text where
  toSelector = ByCSS

-- | Ensure that the element is the only element matching the selector.
select :: CanSelect a => a -> WD Element
select x = do
  -- True  = found more than one element
  -- False = found no elements
  v <- liftIO $ newIORef False
  let findOne = do
        es <- selectAll x
        case es of
          [e] -> return e
          []  -> do liftIO $ writeIORef v False
                    unexpected "select: no elements"
          _   -> do liftIO $ writeIORef v True
                    unexpected "select: more than one element"
  let handler = do
        moreThanOne <- liftIO $ readIORef v
        if moreThanOne
          then expectationFailure $
               printf "%s isn't unique on the page" (show x)
          else expectationFailure $
               printf "%s wasn't found on the page" (show x)
  waitUntil 2 findOne `onTimeout` handler

-- | Select one of the elements matching the selector.
selectSome :: CanSelect a => a -> WD Element
selectSome x = do
  es <- selectAll x
  when (null es) $ expectationFailure $
    printf "%s wasn't found on the page" (show x)
  return (head es)

-- | @font-size@ of an element, in pixels
fontSize :: CanSelect a => a -> WD Double
-- TODO: can fail (NOTE [staleness])
fontSize s = do
  e <- select s
  mbProp <- cssProp e "font-size"
  case mbProp of
    Nothing -> expectationFailure $
                 printf "expected %s to have font-size" (show e)
    Just fs -> case reads (T.toString fs) of
      [(d, "px")] -> return d
      _ -> expectationFailure $
             printf "couldn't parse font-size of %s: %s" (show e) (show fs)

changesURL :: WD a -> WD a
changesURL x = do
  url <- getCurrentURL
  a <- x
  waitUntil 2 (expect =<< ((/= url) <$> getCurrentURL))
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

checkPresent :: CanSelect a => a -> WD ()
checkPresent x = void (select x)

checkPresentSome :: CanSelect a => a -> WD ()
checkPresentSome x = void (selectSome x)

checkNotPresent :: CanSelect a => a -> WD ()
checkNotPresent x = waitUntil 2 $ do
  es <- selectAll x
  when (not (null es)) $ unexpected $
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
          _prerender     = False,
          _discussLink   = Just "http://discuss.link" }
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

_site :: IO ()
_site = run $ do
  session "_" $ using Firefox $ do
    wd "_" $ do
      openGuidePage "/"
      _pause

expectationFailure :: MonadIO m => String -> m a
expectationFailure s = do
  liftIO (Hspec.expectationFailure s)
  undefined

shouldSatisfy :: (Show a, MonadIO m) => a -> (String, a -> Bool) -> m ()
shouldSatisfy a (s, p) = unless (p a) $
  expectationFailure (printf "expected %s to %s" (show a) s)

shouldBeInRange :: (Show a, Ord a, MonadIO m) => a -> (a, a) -> m ()
shouldBeInRange a (x, y) =
  shouldSatisfy a ("be in range " ++ show (x,y), \n -> n >= x && n <= y)

shouldHaveProp :: CanSelect a => a -> (Text, Text) -> WD ()
-- TODO: can fail (NOTE [staleness])
s `shouldHaveProp` (a, txt) = do
  e <- select s
  t <- cssProp e a
  unless (Just txt == t) $ expectationFailure $
    printf "expected property %s of %s to be %s, got %s"
           a (show e) (show txt) (show t)

shouldBeSelected :: CanSelect a => a -> WD ()
-- TODO: can fail (NOTE [staleness])
shouldBeSelected s = do
  e <- select s
  x <- isSelected e
  e `shouldSatisfy` ("be checked/selected", const x)

shouldBeDisplayed :: CanSelect a => a -> WD ()
-- TODO: can fail (NOTE [staleness])
shouldBeDisplayed s = do
  e <- select s
  x <- isDisplayed e
  e `shouldSatisfy` ("be displayed", const x)

shouldBeHidden :: CanSelect a => a -> WD ()
-- TODO: can fail (NOTE [staleness])
shouldBeHidden s = do
  e <- select s
  x <- isDisplayed e
  e `shouldSatisfy` ("be hidden", const (not x))

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
