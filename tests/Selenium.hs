{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Selenium
(
  module Test.Hspec.WebDriver,
  module Test.WebDriver.Commands.Wait,
  wd,
  wait_delay,

  -- * Selecting
  CanSelect(..),
  ComplexSelector(..),
  select,
  selectSome,

  -- * Webdriver
  click,
  cssProp,
  attr,
  enterInput,
  fontSize,
  highlight,
  selectDropdown,
  getBackAfterwards,
  getLink,
  getText,
  getValue,

  -- * Expectations
  changesURL,
  doesNotChangeURL,
  checkPresent,
  checkPresentSome,
  checkNotPresent,
  shouldHaveAttr,
  shouldHaveProp,
  shouldLinkTo,
  shouldSatisfy,
  shouldHaveText,
  shouldNotBe,
  shouldBeHidden,
  shouldBeInRange,
  shouldBeSelected,
  shouldBeDisplayed,
  expectationFailure,

  -- * Keys
  _backspace, _enter, _esc,
  _shift, _ctrl, _alt, _command,
)
where


import BasePrelude hiding (catch, bracket)
-- Lenses
import Lens.Micro.Platform
-- Monads
import Control.Monad.IO.Class
import Control.Monad.Loops
-- Containers
import qualified Data.Set as Set
-- Text
import Data.Text.All (Text)
import qualified Data.Text.All as T
-- Testing
import Test.Hspec.WebDriver hiding
  (getText, shouldHaveAttr, shouldHaveText, click, cssProp, attr)
import qualified Test.Hspec.WebDriver as WD
import Test.WebDriver.Commands.Wait
import Test.WebDriver.Exceptions
import qualified Test.Hspec.Expectations as Hspec
-- Exceptions
import Control.Monad.Catch

-- Local
import Utils (ordNub)


getLink :: CanSelect s => s -> WD String
getLink s = do
  e <- select s
  -- Select all links including the root element itself
  linkElems <- selectAll ((e :& "a") :| (e :// "a"))
  links <- nub . catMaybes <$> mapM (flip attr "href") linkElems
  case links of
    [x] -> return (T.toString x)
    []  -> expectationFailure $
             printf "expected %s to contain a link" (show s)
    _   -> expectationFailure $
             printf "expected %s to contain only one link" (show s)

getText :: CanSelect s => s -> WD Text
getText s = do
  -- TODO: Note [staleness]
  e <- select s
  WD.getText e

getValue :: CanSelect s => s -> WD Text
getValue x = fromMaybe "" <$> attr x "value"

enterInput :: CanSelect s => Text -> s -> WD ()
enterInput x s = do
  input <- select s
  clearInput input
  sendKeys (x <> _enter) input
  checkNotPresent input

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

shouldLinkTo :: CanSelect a => a -> String -> WD ()
s `shouldLinkTo` url2 = do
  url <- getLink s
  url `shouldBe` url2

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
     \for (var i=0;i<sel.options.length;i++) {\
     \  if (sel.options[i]==opt) {\
     \    sel.selectedIndex=i;\
     \    sel.onchange && sel.onchange();\
     \    break; }}"
   :: WD (Maybe ()))

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
  -- | Only pick the Nth (starting from 0) selected element
  Index :: CanSelect a => Int -> a -> ComplexSelector
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
      Take  n a -> take n <$> selectAll a
      Index n a -> toListOf (ix n) <$> selectAll a
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
  waitUntil wait_delay findOne `onTimeout` handler

-- | Select one of the elements matching the selector.
selectSome :: CanSelect a => a -> WD Element
selectSome x = do
  es <- selectAll x
  when (null es) $ expectationFailure $
    printf "%s wasn't found on the page" (show x)
  return (head es)

-- | @font-size@ of an element, in pixels
fontSize :: CanSelect a => a -> WD Double
fontSize s = do
  mbProp <- cssProp s "font-size"
  case mbProp of
    Nothing -> expectationFailure $
                 printf "expected %s to have font-size" (show s)
    Just fs -> case reads (T.toString fs) of
      [(d, "px")] -> return d
      _ -> expectationFailure $
             printf "couldn't parse font-size of %s: %s" (show s) (show fs)

cssProp :: CanSelect a => a -> Text -> WD (Maybe Text)
-- TODO: can fail (NOTE [staleness])
cssProp s p = do
  e <- select s
  WD.cssProp e p

attr :: CanSelect a => a -> Text -> WD (Maybe Text)
-- TODO: can fail (NOTE [staleness])
attr s p = do
  e <- select s
  WD.attr e p

changesURL :: WD a -> WD a
changesURL x = do
  url <- getCurrentURL
  a <- x
  waitUntil wait_delay (expect =<< ((/= url) <$> getCurrentURL))
  return a

doesNotChangeURL :: WD a -> WD a
doesNotChangeURL x = do
  url <- getCurrentURL
  a <- x
  -- TODO: somehow check that the browser isn't even trying to change the URL
  url2 <- getCurrentURL
  url2 `shouldBe` url
  return a

getBackAfterwards :: WD a -> WD a
getBackAfterwards x = do
  url <- getCurrentURL
  a <- x
  openPage url
  return a

wd :: String -> WD a -> SpecWith (WdTestSession ())
wd x act = it x (runWD (void act))

checkPresent :: CanSelect a => a -> WD ()
checkPresent x = void (select x)

checkPresentSome :: CanSelect a => a -> WD ()
checkPresentSome x = void (selectSome x)

checkNotPresent :: CanSelect a => a -> WD ()
checkNotPresent x = waitUntil wait_delay $ do
  es <- selectAll x
  when (not (null es)) $ unexpected $
    printf "expected %s not to be present on the page" (show x)

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

shouldNotBe :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldNotBe a x =
  shouldSatisfy a ("not be " ++ show x, const (a /= x))

shouldHaveProp :: CanSelect a => a -> (Text, Text) -> WD ()
s `shouldHaveProp` (a, txt) = do
  t <- cssProp s a
  unless (Just txt == t) $ expectationFailure $
    printf "expected property %s of %s to be %s, got %s"
           a (show s) (show txt) (show t)

shouldBeSelected :: CanSelect a => a -> WD ()
-- TODO: can fail (NOTE [staleness])
shouldBeSelected s = do
  e <- select s
  x <- isSelected e
  s `shouldSatisfy` ("be checked/selected", const x)

shouldBeDisplayed :: CanSelect a => a -> WD ()
-- TODO: can fail (NOTE [staleness])
shouldBeDisplayed s = do
  e <- select s
  x <- isDisplayed e
  s `shouldSatisfy` ("be displayed", const x)

shouldBeHidden :: CanSelect a => a -> WD ()
-- TODO: can fail (NOTE [staleness])
shouldBeHidden s = do
  e <- select s
  x <- isDisplayed e
  s `shouldSatisfy` ("be hidden", const (not x))

_backspace, _enter, _esc :: Text
(_backspace, _enter, _esc) = ("\xE003", "\xE007", "\xE00C")
_shift, _ctrl, _alt, _command :: Text
(_shift, _ctrl, _alt, _command) = ("\xE008", "\xE009", "\xE00A", "\xE03D")

wait_delay :: Double
wait_delay = 5

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
