{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module WebSpec (tests) where


import BasePrelude hiding (catch, bracket)
-- Monads
import Control.Monad.IO.Class
import Control.Monad.Loops
-- Concurrency
import qualified SlaveThread as Slave
-- Text
import Data.Text.All (Text)
import qualified Data.Text.All as T
-- Files
import System.Directory
-- URLs
import Network.URI
-- Exceptions
import Control.Monad.Catch

-- Testing
import Selenium

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
  itemTests
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
      waitUntil wait_delay (expect . inRange (90, 140) . snd =<< elemSize footer)
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
      titleLink `shouldLinkToRelative` "/haskell"
    wd "has a subtitle" $ do
      checkPresent ".subtitle"
    wd "doesn't have an add-category field" $ do
      checkNotPresent ".add-category"
    wd "is present on the main page" $ do
      getBackAfterwards $ do
        catURL <- getCurrentURL
        openGuidePage "/"
        ByLinkText "Some category" `shouldLinkTo` catURL
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
        enterInput "Cat 1" (form :// ByName "title")
        categoryTitle `shouldHaveText` "Cat 1"
      wd "changes page slug when changed" $ do
        changesURL $ refresh
        do (slug, _) <- parseCategoryURL . uriPath =<< getCurrentRelativeURL
           slug `shouldBe` "cat-1"
        form <- openCategoryEditForm
        enterInput "Cat 2" (form :// ByName "title")
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
        enterInput "Basics" (form :// ByName "group")
        categoryGroup `shouldHaveText` "Basics"
      wd "is changed on the front page too" $ do
        onAnotherPage "/" $ do
          catLink <- select (ByLinkText "Cat 2")
          groupHeader <- select ((".category-group" :<// catLink) :// "h2")
          groupHeader `shouldHaveText` "Basics"
    describe "status" $ do
      wd "is “stub” by default" $ do
        form <- openCategoryEditForm
        chosenOption <- select (form :// ByName "status" :// "option:checked")
        chosenOption `shouldHaveText` "Stub"
        onAnotherPage "/" $ do
          ByLinkText "Cat 2" `shouldHaveAttr` ("class", "status-stub")
      wd "can be changed" $ do
        form <- openCategoryEditForm
        sel <- select (form :// ByName "status")
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
        checkbox <- select (form :// ByName "pros-cons-enabled")
        shouldBeSelected checkbox
        click (form :// ".cancel")
      wd "section is shown in an item" $ do
        mapM_ shouldBeDisplayed =<< selectAll ".item-traits"
      wd "section isn't shown after unchecking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// ByName "pros-cons-enabled")
        click (form :// ".save")
        waitUntil wait_delay $
          expect . not =<< anyM isDisplayed =<< selectAll ".item-traits"
      wd "section is shown again after checking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// ByName "pros-cons-enabled")
        click (form :// ".save")
        waitUntil wait_delay $
          expect =<< allM isDisplayed =<< selectAll ".item-traits"
    describe "ecosystem enabled" $ do
      wd "checkbox enabled by default" $ do
        form <- openCategoryEditForm
        checkbox <- select (form :// ByName "ecosystem-enabled")
        shouldBeSelected checkbox
        click (form :// ".cancel")
      wd "section is shown in an item" $ do
        mapM_ shouldBeDisplayed =<< selectAll ".item-ecosystem"
      wd "section isn't shown after unchecking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// ByName "ecosystem-enabled")
        click (form :// ".save")
        waitUntil wait_delay $
          expect . not =<< anyM isDisplayed =<< selectAll ".item-ecosystem"
      wd "section is shown again after checking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// ByName "ecosystem-enabled")
        click (form :// ".save")
        waitUntil wait_delay $
          expect =<< allM isDisplayed =<< selectAll ".item-ecosystem"
  describe "feed" $ do
    -- TODO: actually test the generated feed
    wd "exists" $ do
      getBackAfterwards $ do
        url <- getCurrentRelativeURL
        (_, catId) <- parseCategoryURL (uriPath url)
        ".category-feed" `shouldLinkToRelative`
          ("/haskell/feed/category/" <> catId)
        click ".category-feed"
        checkPresent (".item-name" :& HasText "some item")
        checkPresent (".item-name" :& HasText "another item")
  describe "description" $ do
    wd "has a default template" $ do
      click (ByLinkText "edit description")
      contents <- T.lines <$> getValue ".category-notes textarea"
      contents `shouldSatisfy`
        ("have “# Recommendations”", ("# Recommendations" `elem`))
    -- TODO: editing works
  describe "deleting a category" $ do
    wd "dismissing the alert doesn't do anything" $ do
      click (".category h2" :// ByLinkText "delete")
      dismissAlert
      getBackAfterwards $ do
        catURL <- getCurrentURL
        openGuidePage "/"
        ByLinkText "Cat 2" `shouldLinkTo` catURL
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

itemTests :: Spec
itemTests = session "items" $ using Firefox $ do
  openGuide "/"
  wd "create a test category" $ do
    createCategory "Item test category"
  wd "add a new item" $ do
    createItem "An item"
  let item1 = Index 0 ".item"
  describe "item properties" $ do
    describe "name" $ do
      wd "is present" $ do
        itemName item1 `shouldHaveText` "An item"
        fs <- fontSize (itemName item1); fs `shouldBeInRange` (20,26)
      wd "doesn't link to Hackage" $ do
        doesNotChangeURL $ click (itemName item1)
        -- TODO: find a better test for this (maybe by checking all hrefs)
        checkNotPresent (item1 :// ByLinkText "Hackage")
      wd "can be changed" $ do
        form <- openItemEditForm item1
        enterInput "New item" (form :// ByName "name")
        itemName item1 `shouldHaveText` "New item"
      wd "doesn't link to Hackage if changed to something without spaces" $ do
        form <- openItemEditForm item1
        enterInput "item1" (form :// ByName "name")
        itemName item1 `shouldHaveText` "item1"
        doesNotChangeURL $ click (itemName item1)
        checkNotPresent (item1 :// ByLinkText "Hackage")
      wd "links to Hackage if the name is originally a package name" $ do
        item2 <- createItem "foo-bar-2"
        itemName item2 `shouldHaveText` "foo-bar-2"
        (item2 :// ByLinkText "Hackage")
          `shouldLinkTo` "https://hackage.haskell.org/package/foo-bar-2"
    describe "group" $ do
      wd "is present and “other” by default" $ do
        itemGroup item1 `shouldHaveText` "other"
        fs <- fontSize (itemGroup item1); fs `shouldBeInRange` (15,17)
        form <- openItemEditForm item1
        (form :// ByName "group" :// ":checked") `shouldHaveText` "-"
        click (form :// ".cancel")
      wd "custom group input is hidden but then shows" $ do
        form <- openItemEditForm item1
        sel <- select (form :// ByName "group")
        opt <- select (sel :// HasText "New group...")
        shouldBeHidden (form :// ByName "custom-group")
        selectDropdown sel opt
        shouldBeDisplayed (form :// ByName "custom-group")
        click (form :// ".cancel")
      wd "can be changed to a custom group" $ do
        setItemCustomGroup "some group" item1
      -- TODO: check that it works with 2 groups etc
      wd "is automatically put into all items' choosers" $ do
        -- TODO: make a combinator for this
        items <- selectAll ".item"
        waitUntil wait_delay $ expect (length items >= 2)
        for_ items $ \item -> do
          form <- openItemEditForm item
          checkPresent $
            form :// ByName "group" :// "option" :& HasText "some group"
          click (form :// ".cancel")
      wd "is present in the chooser after a refresh" $ do
        refresh
        form <- openItemEditForm item1
        sel <- select (form :// ByName "group")
        (sel :// ":checked") `shouldHaveText` "some group"
        click (form :// ".cancel")
        -- TODO: more convoluted change scenarious
      -- TODO: setting custom group to something that already exists
      -- doesn't result in two equal groups
      wd "changing it changes the color" $ do
        [itemA, itemB, itemC] <- replicateM 3 (createItem "blah")
        setItemCustomGroup "one" itemA
        setItemGroup "one" itemB
        setItemCustomGroup "two" itemC
        let getColors = for [itemA, itemB, itemC] $ \item ->
              (,) <$> cssProp (item :// ".item-info") "background-color"
                  <*> cssProp (item :// ".item-body") "background-color"
        -- A=1,B=1,C=2; check that A=B, A≠C
        do [aCol, bCol, cCol] <- getColors
           aCol `shouldBe` bCol; aCol `shouldNotBe` cCol
        -- A:=2; now A=2,B=1,C=2; check that A≠B, A=C
        setItemCustomGroup "two" itemA
        do [aCol, bCol, cCol] <- getColors
           aCol `shouldNotBe` bCol; aCol `shouldBe` cCol
        -- C:=1; now A=2,B=1,C=1; check that A≠C, B=C
        setItemGroup "one" itemC
        do [aCol, bCol, cCol] <- getColors
           aCol `shouldNotBe` cCol; bCol `shouldBe` cCol
        
    -- TODO: kind
    -- TODO: site
  describe "items with the same name" $ do
    wd "can be present" $ do
      createItem "item1"
      waitUntil wait_delay $
        expect . (== 2) . length =<< selectAll
          (itemName ".item" :& HasText "item1")
    wd "can be changed separately" $ do
      item2 <- select $
        Index 1 (".item" :<// (".item-name" :& HasText "item1"))
      form <- openItemEditForm item2
      enterInput "Blah" (form :// ByName "name")
      itemName item1 `shouldHaveText` "item1"
      itemName item2 `shouldHaveText` "Blah"
  -- TODO: moving item up/down
  -- TODO: deleting an item
  -- TODO: pros/cons
  -- TODO: summary
  -- TODO: ecosystem
  -- TODO: notes
  -- TODO: item's self-link in the header

markdownTests :: Spec
markdownTests = session "markdown" $ using Firefox $ do
  openGuide "/"
  describe "Markdown isn't allowed in category names" $ do
    wd "when creating a category" $ do
      createCategory "*foo*"
      categoryTitle `shouldHaveText` "*foo*"
    wd "when changing existing category's name" $ do
      form <- openCategoryEditForm
      enterInput "foo `bar`" (form :// ByName "title")
      categoryTitle `shouldHaveText` "foo `bar`"
  -- TODO: check that headers in notes Markdown are rendered as headers but
  -- still have smaller font size

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
  waitUntil wait_delay (expect . (\xs -> length xs > length items) =<< selectItems)
  items2 <- selectItems
  case items2 \\ items of
    [] -> expectationFailure "an item wasn't created"
    [x] -> return x
    _ -> expectationFailure "more than one item was created"

itemName :: CanSelect s => s -> ComplexSelector
itemName item = item :// ".item-name"

itemGroup :: CanSelect s => s -> ComplexSelector
itemGroup item = item :// ".item-group"

setItemGroup :: CanSelect s => Text -> s -> WD ()
setItemGroup g item = do
  form <- openItemEditForm item
  sel <- select (form :// ByName "group")
  opt <- select (sel :// HasText g)
  selectDropdown sel opt
  click (form :// ".save")
  itemGroup item `shouldHaveText` g

setItemCustomGroup :: CanSelect s => Text -> s -> WD ()
setItemCustomGroup g item = do
  form <- openItemEditForm item
  sel <- select (form :// ByName "group")
  opt <- select (sel :// HasText "New group...")
  selectDropdown sel opt
  enterInput g (form :// ByName "custom-group")
  itemGroup item `shouldHaveText` g

categoryTitle :: Selector
categoryTitle = ByCSS ".category-title"

categoryGroup :: Selector
categoryGroup = ByCSS ".category .group"

openCategoryEditForm :: WD Element
openCategoryEditForm = do
  click (".category h2" :// ByLinkText "edit")
  select ".category-info form"

openItemEditForm :: CanSelect s => s -> WD Element
openItemEditForm item = do
  click (item :// ".edit-item-info")
  select (item :// ".item-info form")

-----------------------------------------------------------------------------
-- Utilities for webdriver
-----------------------------------------------------------------------------

_TODO :: MonadIO m => m ()
_TODO = error "test not implemented"

_pause :: WD ()
_pause = do
  liftIO $ putStr "press Enter to continue testing, or “q” to quit: "
  x <- liftIO $ getLine
  when (x == "q") $
    expectationFailure "quit"

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

shouldLinkToRelative :: CanSelect a => a -> String -> WD ()
s `shouldLinkToRelative` url2 = do
  -- TODO: would be nice if it checked relative to the current page
  url <- getLink s
  case parseURI url of
    Nothing -> error ("couldn't parse as URL: " ++ url)
    Just u  -> do
      maybe "" uriRegName (uriAuthority u) `shouldBe` "localhost"
      uriPath u `shouldBe` url2
