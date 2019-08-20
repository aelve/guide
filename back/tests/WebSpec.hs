{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds      #-}


module WebSpec (tests) where

-- Shared imports
import Imports
-- Monads
import Control.Monad.Loops
-- Concurrency
import Control.Concurrent.Async (withAsync)
-- Text
import qualified Data.Text as T
-- Files
import System.IO.Temp
-- URLs
import Network.URI
-- Testing
import qualified ApiSpec
import qualified LogSpec
import Selenium
import qualified Test.WebDriver.Common.Keys as Key
-- Site
import Guide.Config (Config (..), def)
import qualified Guide.Main


-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

tests :: IO ()
tests = withSystemTempFile "test_guide.log" $ \logFile logFileHandle -> do
  -- Close the log file because otherwise 'run' won't be able to open it
  hClose logFileHandle
  run logFile $ do
    mainPageTests
    categoryTests
    itemTests
    markdownTests
    ApiSpec.tests
  hspec $
    LogSpec.tests logFile
  -- TODO: ApiSpec, LogSpec, and WebSpec should be independent of each
  -- other. Currently it's a mess.

mainPageTests :: Spec
mainPageTests = session "main page" $ using [chromeCaps] $ do
  openGuide "/"
  wd "is initially empty" $ do
    checkPresent "#categories"
    checkNotPresent "#categories > *"
  wd "has a google-token" $ do
    "meta[name=google-site-verification]" `shouldHaveAttr`
      ("content", "some-google-token")
  wd "has a title" $ do
    "h1" `shouldHaveText` "Aelve Guide | Haskell"
  -- describe "subtitle" $ do
  --   wd "is present" $ do
  --     sub <- select ".subtitle"
  --     fs <- fontSize sub; fs `shouldBeInRange` (15,17)
  --   wd "has a discuss link" $ do
  --     checkPresent ".subtitle a[href='http://discuss.link']"

categoryTests :: Spec
categoryTests = session "categories" $ using [chromeCaps] $ do
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
      titleLink `shouldHaveText` "Aelve Guide | Haskell"
      titleLink `shouldLinkToRelative` "/haskell"
    -- wd "has a subtitle" $ do
    --   checkPresent ".subtitle"
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
          ("div" :</ ("p" :</ ByLinkText "Cat 2")) `shouldHaveAttr`
            ("class", "categories-stub")
      wd "can be changed" $ do
        form <- openCategoryEditForm
        sel <- select (form :// ByName "status")
        opt <- select (sel :// HasText "Complete")
        selectDropdown sel opt
        saveForm form
        onAnotherPage "/" $ do
          ("div" :</ ByLinkText "Cat 2") `shouldHaveAttr`
            ("class", "categories-finished")
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
        saveForm form
        waitUntil wait_delay $
          expect . not =<< anyM isDisplayed =<< selectAll ".item-traits"
      wd "section is shown again after checking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// ByName "pros-cons-enabled")
        saveForm form
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
        saveForm form
        waitUntil wait_delay $
          expect . not =<< anyM isDisplayed =<< selectAll ".item-ecosystem"
      wd "section is shown again after checking the checkbox" $ do
        form <- openCategoryEditForm
        click (form :// ByName "ecosystem-enabled")
        saveForm form
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
        -- commented out because it doesn't work in Chrome
        -- checkPresent (".item-name" :& HasText "some item")
        -- checkPresent (".item-name" :& HasText "another item")
  describe "notes" $ do
    wd "has a default template" $ do
      form <- openCategoryNotesEditForm
      contents <- T.lines <$> getValue (form :// "textarea")
      contents `shouldSatisfy`
        ("have “# Recommendations”", ("# Recommendations" `elem`))
      click (form :// ".cancel")
    wd "can be edited" $ do
      form <- openCategoryNotesEditForm
      setInput "Blah blah" (form :// "textarea")
      saveForm form
      ".category-notes .notes-like" `shouldHaveText` "Blah blah"
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
      -- TODO: we should have, like, a 404 page
      "body" `shouldHaveText` "404 - Not Found\npowered by Spock"

itemTests :: Spec
itemTests = session "items" $ using [chromeCaps] $ do
  openGuide "/"
  wd "create a test category" $ do
    createCategory "Item test category"
  wd "add new items" $ do
    createItem "An item"
  let item1 = Index 0 ".item"

  describe "item properties" $ do
    describe "name" $ do
      wd "is present" $ do
        itemName item1 `shouldHaveText` "An item"
        fs <- fontSize (itemName item1); fs `shouldBeInRange` (20,26)
      wd "can be changed" $ do
        form <- openItemEditForm item1
        enterInput "New item" (form :// ByName "name")
        itemName item1 `shouldHaveText` "New item"
    -- TODO: kind
    -- TODO: site

  describe "item sections" $ do
    describe "description/summary" $ do
      wd "default state" $ do
        (item1 :// ".item-description .notes-like p") `shouldHaveText`
          "write something here!"
        form <- openItemDescriptionEditForm item1
        val <- getValue (form :// "textarea")
        val `shouldBe` ""
        click (form :// ".cancel")
      wd "can be changed" $ do
        do form <- openItemDescriptionEditForm item1
           setInput "foo *bar*\n\n# Blah" (form :// "textarea")
           saveForm form
        section <- select (item1 :// ".item-description .notes-like")
        (section :// "p")  `shouldHaveText` "foo bar"
        (section :// "h1") `shouldHaveText` "Blah"
        do form <- openItemDescriptionEditForm item1
           val <- getValue (form :// "textarea")
           val `shouldBe` "foo *bar*\n\n# Blah"
           click (form :// ".cancel")
      wd "edit-cancel-edit works" $ do
        -- It should not store the edited text if it wasn't saved
        do form <- openItemDescriptionEditForm item1
           setInput "ehhhh" (form :// "textarea")
           click (form :// ".cancel")
        do form <- openItemDescriptionEditForm item1
           val <- getValue (form :// "textarea")
           val `shouldBe` "foo *bar*\n\n# Blah"
           click (form :// ".cancel")
    -- TODO: pros/cons
    describe "ecosystem" $ do
      wd "default state" $ do
        (item1 :// ".item-ecosystem .notes-like") `shouldHaveText` ""
        form <- openItemEcosystemEditForm item1
        val <- getValue (form :// "textarea")
        val `shouldBe` ""
        click (form :// ".cancel")
      wd "can be changed" $ do
        do form <- openItemEcosystemEditForm item1
           setInput "foo *bar*\n\n# Blah" (form :// "textarea")
           saveForm form
        section <- select (item1 :// ".item-ecosystem .notes-like")
        (section :// "p")  `shouldHaveText` "foo bar"
        (section :// "h1") `shouldHaveText` "Blah"
        do form <- openItemEcosystemEditForm item1
           val <- getValue (form :// "textarea")
           val `shouldBe` "foo *bar*\n\n# Blah"
           click (form :// ".cancel")
      wd "edit-cancel-edit works" $ do
        -- It should not store the edited text if it wasn't saved
        do form <- openItemEcosystemEditForm item1
           setInput "ehhhh" (form :// "textarea")
           click (form :// ".cancel")
        do form <- openItemEcosystemEditForm item1
           val <- getValue (form :// "textarea")
           val `shouldBe` "foo *bar*\n\n# Blah"
           click (form :// ".cancel")
    -- TODO: notes

  describe "items with the same name" $ do
    wd "can be present" $ do
      createItem "same name"
      createItem "same name"
      waitUntil wait_delay $
        expect . (== 2) . length =<< selectAll
          (itemName ".item" :& HasText "same name")
    wd "can be changed separately" $ do
      itemA <- select $
        Index 0 (".item" :<// (".item-name" :& HasText "same name"))
      itemB <- select $
        Index 1 (".item" :<// (".item-name" :& HasText "same name"))
      form <- openItemEditForm itemB
      enterInput "Blah" (form :// ByName "name")
      itemName itemA `shouldHaveText` "same name"
      itemName itemB `shouldHaveText` "Blah"
  describe "moving items" $ do
    let getId :: CanSelect a => a -> WD Text
        getId x = Selenium.attr x "id" >>= \case
          Nothing -> expectationFailure $
                       printf "expected %s to have an id" (show x)
          Just i  -> return i
    wd "up" $ do
      ids <- mapM getId =<< selectAll ".item"
      click (ById (ids !! 1) :// ".move-item-up")
      waitWhile 0.10 (return ()) `onTimeout` return ()
      ids2 <- mapM getId =<< selectAll ".item"
      ids2 `shouldBe` (ids !! 1 : ids !! 0 : drop 2 ids)
    -- TODO: select should only select visible elements
    -- TODO: try to move the first item up
    -- TODO: down
  -- TODO: item's self-link in the header
  -- TODO: deleting an item

-- TODO: tests for merging-on-conflicts

markdownTests :: Spec
markdownTests = session "markdown" $ using [chromeCaps] $ do
  openGuide "/"
  describe "Markdown isn't allowed in category names" $ do
    wd "when creating a category" $ do
      createCategory "*foo*"
      categoryTitle `shouldHaveText` "*foo*"
    wd "when changing existing category's name" $ do
      form <- openCategoryEditForm
      enterInput "foo `bar`" (form :// ByName "title")
      categoryTitle `shouldHaveText` "foo `bar`"
  wd "Markdown in category notes" $ do
    form <- openCategoryNotesEditForm
    setInput "# Test\n*foo*" (form :// "textarea")
    saveForm form
    ".category-notes .notes-like h1" `shouldHaveText` "Test"
    ".category-notes .notes-like p em" `shouldHaveText` "foo"
  describe "Markdown in item descriptions" $ do
    let item = Index 0 ".item"
    wd "works" $ do
      createItem "test"
      form <- openItemDescriptionEditForm item
      setInput "# Blah\n*bar*" (form :// "textarea")
      saveForm form
      (item :// ".item-description .notes-like h1") `shouldHaveText` "Blah"
      (item :// ".item-description .notes-like p em") `shouldHaveText` "bar"
    wd "@hk links are parsed as Hackage links" $ do
      form <- openItemDescriptionEditForm item
      setInput "[foo-bar](@hk)" (form :// "textarea")
      saveForm form
      (item :// ".item-description .notes-like a") `shouldHaveText` "foo-bar"
      (item :// ".item-description .notes-like a") `shouldLinkTo`
        "https://hackage.haskell.org/package/foo-bar"
  -- TODO: check that headers in notes Markdown are rendered as headers but
  -- still have smaller font size

-----------------------------------------------------------------------------
-- Helpers dealing with guide specifically
-----------------------------------------------------------------------------

parseCategoryURL :: String -> WD (String, String)
parseCategoryURL url = do
  case T.stripPrefix "/haskell/" (toText url) of
    Nothing -> expectationFailure $
                 printf "%s doesn't start with /haskell/" (show url)
    Just u -> do
      let (slug, catId) = T.breakOnEnd "-" u
      slug `shouldSatisfy` ("not null", not . T.null)
      T.last slug `shouldBe` '-'
      return (toString (T.init slug), toString catId)

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
  changesURL $ enterInput t ".add-category"

-- Assumes that the category page is open
createItem :: Text -> WD Element
createItem t = do
  let selectItems = selectAll ".item"
  items <- selectItems
  sendKeys (t <> Key.enter) ".add-item"
  waitUntil wait_delay (expect . (\xs -> length xs > length items) =<< selectItems)
  items2 <- selectItems
  case items2 \\ items of
    []  -> expectationFailure "an item wasn't created"
    [x] -> return x
    _   -> expectationFailure "more than one item was created"

itemName :: CanSelect s => s -> ComplexSelector
itemName item = item :// ".item-name"

categoryTitle :: Selector
categoryTitle = ByCSS ".category-title"

categoryGroup :: Selector
categoryGroup = ByCSS ".category .group"

openCategoryEditForm :: WD Element
openCategoryEditForm = do
  click (".category h2" :// ByLinkText "edit")
  select ".category-info form"

openCategoryNotesEditForm :: WD Element
openCategoryNotesEditForm = do
  click (".category-notes" :// ByLinkText "edit description")
  select ".category-notes .editing"

openItemEditForm :: CanSelect s => s -> WD Element
openItemEditForm item = do
  click (item :// ".edit-item-info")
  select (item :// ".item-info form")

openItemDescriptionEditForm :: CanSelect s => s -> WD Element
openItemDescriptionEditForm item = do
  click (item :// ".item-description .normal .edit-item-description")
  select (item :// ".item-description .editing")

openItemEcosystemEditForm :: CanSelect s => s -> WD Element
openItemEcosystemEditForm item = do
  click (item :// ".item-ecosystem .normal .edit-item-ecosystem")
  select (item :// ".item-ecosystem .editing")

-- | Save a form and wait for it to close.
--
-- Assumes that the form has a button with class @save@.
saveForm :: CanSelect s => s -> WD ()
saveForm form = do
  click (form :// ".save")
  -- Normally Selenium wouldn't wait for the “save” request to complete, so
  -- we have to manually wait until the form is hidden. It's important
  -- because otherwise things like issue #134 happen (when Selenium asks for
  -- another page before the form has finished saving).
  checkNotPresent form

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

-- 'Run' prepares directories and config to launch site server for spec tests
-- and closes them all after test finished.
run :: FilePath -> Spec -> IO ()
run logFile ts = do
  -- Config to run spock server.
  let config = def {
        baseUrl       = "/",
        googleToken   = "some-google-token",
        adminPassword = "123",
        discussLink   = Just "http://discuss.link",
        cors          = False,
        logToStderr   = False,
        logToFile     = Just logFile
        }
  -- Prepere resources.
  let prepare = do
        exold <- doesDirectoryExist "state-old"
        when exold $ error "state-old exists"
        ex <- doesDirectoryExist "state"
        when ex $ renameDirectory "state" "state-old"

  -- Release resources.
  let finish _ = do
        ex' <- doesDirectoryExist "state"
        when ex' $ removeDirectoryRecursive "state"
        exold' <- doesDirectoryExist "state-old"
        when exold' $ renameDirectory "state-old" "state"

  bracket prepare finish $ \_ -> do
    withAsync (Guide.Main.runServer config) $ \_ -> hspec ts

_site :: IO ()
_site = run "" $ do
  session "_" $ using [chromeCaps] $ do
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
