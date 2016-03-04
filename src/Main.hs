{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
RankNTypes,
FlexibleInstances,
FlexibleContexts,
QuasiQuotes,
ScopedTypeVariables,
FunctionalDependencies,
GeneralizedNewtypeDeriving,
TypeFamilies,
DataKinds,
MultiWayIf,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude hiding (Category)
-- Monads and monad transformers
import Control.Monad.State
-- Lenses
import Lens.Micro.Platform
-- Containers
import qualified Data.Map as M
import Data.Map (Map)
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation
import qualified Data.Text.Buildable as Format
-- Randomness
import System.Random
-- Web
import Lucid hiding (for_)
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Network.Wai.Middleware.Static
import Web.PathPieces

-- Local
import JS (JS(..), ToJS, allJSFunctions)
import qualified JS
import Utils


-- | Unique id, used for many things – categories, items, and anchor ids.
-- Note that in HTML 5 using numeric ids for divs, spans, etc is okay.
newtype Uid = Uid {uidToText :: Text}
  deriving (Eq, PathPiece, ToJS, Format.Buildable)

instance IsString Uid where
  fromString = Uid . T.pack

randomUid :: MonadIO m => m Uid
randomUid = liftIO $ Uid . tshow <$> randomRIO (0::Int, 10^(9::Int))

data Trait = Trait {
  _traitUid :: Uid,
  _traitContent :: Text }

makeFields ''Trait

data ItemKind
  = Library {_itemKindOnHackage :: Bool}
  | Other
  deriving (Eq, Show)

hackageLibrary :: ItemKind
hackageLibrary = Library True

makeFields ''ItemKind

data Item = Item {
  _itemUid    :: Uid,
  _itemName   :: Text,
  _itemGroup_ :: Maybe Text,
  _itemPros   :: [Trait],
  _itemCons   :: [Trait],
  _itemLink   :: Maybe Url,
  _itemKind   :: ItemKind }

makeFields ''Item

traitById :: Uid -> Lens' Item Trait
traitById uid' = singular $
  (pros.each . filtered ((== uid') . view uid)) `failing`
  (cons.each . filtered ((== uid') . view uid))

data Hue = NoHue | Hue Int
  deriving (Eq, Ord)

instance Show Hue where
  show NoHue   = "0"
  show (Hue n) = show n

{-
https://www.google.com/design/spec/style/color.html#color-color-palette

                50     100    200
              ------ ------ ------
red         : FFEBEE FFCDD2 EF9A9A
pink        : FCE4EC F8BBD0 F48FB1
purple      : F3E5F5 E1BEE7 CE93D8
deep purple : EDE7F6 D1C4E9 B39DDB
indigo      : E8EAF6 C5CAE9 9FA8DA
blue        : E3F2FD BBDEFB 90CAF9
light blue  : E1F5FE B3E5FC 81D4FA
cyan        : E0F7FA B2EBF2 80DEEA
teal        : E0F2F1 B2DFDB 80CBC4
green       : E8F5E9 C8E6C9 A5D6A7
light green : F1F8E9 DCEDC8 C5E1A5
lime        : F9FBE7 F0F4C3 E6EE9C
yellow      : FFFDE7 FFF9C4 FFF59D
amber       : FFF8E1 FFECB3 FFE082
orange      : FFF3E0 FFE0B2 FFCC80
deep orange : FBE9E7 FFCCBC FFAB91
brown       : EFEBE9 D7CCC8 BCAAA4
gray        : FAFAFA F5F5F5 EEEEEE
blue gray   : ECEFF1 CFD8DC B0BEC5
-}

-- TODO: more colors and don't repeat them!
-- TODO: what about colorblind people?
-- TODO: check how all colors look (not just deep purple)
hueToDarkColor :: Hue -> Text
hueToDarkColor NoHue = "#D6D6D6"  -- the color for gray isn't from Google's
                                  -- palette, since their “100” is too light
hueToDarkColor (Hue i) = table !! ((i-1) `mod` length table)
  where
    -- the “100” colors
    table = ["#D1C4E9",   -- deep purple
             "#C8E6C9",   -- green
             "#FFECB3",   -- amber
             "#FFCDD2"]   -- red

hueToLightColor :: Hue -> Text
hueToLightColor NoHue = "#F0F0F0"  -- the color for gray isn't from Google's
                                   -- palette, since their “50” is too light
hueToLightColor (Hue i) = table !! ((i-1) `mod` length table)
  where
    -- the “50” colors
    table = ["#EDE7F6",
             "#E8F5E9",
             "#FFF8E1",
             "#FFEBEE"]

data Category = Category {
  _categoryUid :: Uid,
  _categoryTitle :: Text,
  _categoryNotes :: Text,
  _categoryGroups :: Map Text Hue,
  _categoryItems :: [Item] }

makeFields ''Category

addGroupIfDoesNotExist :: Text -> Map Text Hue -> Map Text Hue
addGroupIfDoesNotExist g gs
  | M.member g gs = gs
  | otherwise     = M.insert g firstNotTaken gs
  where
    firstNotTaken = head $ map Hue [1..] \\ M.elems gs

data GlobalState = GlobalState {
  _categories :: [Category] }

makeLenses ''GlobalState

categoryById :: Uid -> Lens' GlobalState Category
categoryById catId = singular $
  categories.each . filtered ((== catId) . view uid)

categoryByItem :: Uid -> Lens' GlobalState Category
categoryByItem itemId = singular $
  categories.each . filtered hasItem
  where
    hasItem category = itemId `elem` (category^..items.each.uid)

itemById :: Uid -> Lens' GlobalState Item
itemById itemId = singular $
  categories.each . items.each . filtered ((== itemId) . view uid)

emptyState :: GlobalState
emptyState = GlobalState {
  _categories = [] }

sampleState :: GlobalState
sampleState = do
  let lensItem = Item {
        _itemUid = "12",
        _itemName = "lens",
        _itemGroup_ = Nothing,
        _itemPros = [
           Trait "121" [text|
             The most widely used lenses library, by a huge margin.|],
           Trait "123" $ T.unwords $ T.lines [text|
             Contains pretty much everything you could want – while other
             lens libraries mostly only provide lenses for manipulating
             lists, maps, tuples, and standard types like
             `Maybe`/`Either`/etc, lens has functions for manipulating
             filepaths, Template Haskell structures, generics, complex
             numbers, exceptions, and everything else in the Haskell
             Platform.|],
           Trait "125" $ T.unwords $ T.lines [text|
             Unlike most other libraries, has prisms – a kind of lenses
             that can act both as constructors and deconstructors at once.
             They can be pretty useful when you're dealing with exceptions,
             Template Haskell, or JSON.|] ],
        _itemCons = [
           Trait "122" $ T.unwords $ T.lines [text|
             Takes a lot of time to compile, and has a lot of dependencies
             as well.|],
           Trait "124" $ T.unwords $ T.lines [text|
             Some of its advanced features are very intimidating, and the
             whole library may seem overengineered
             (see [this post](http://fvisser.nl/post/2013/okt/11/why-i-dont-like-the-lens-library.html)).|],
           Trait "126" $ T.unwords $ T.lines [text|
             Once you start using lenses for *everything* (which is easier
             to do with lens than with other libraries), your code may start
             not looking like Haskell much
             (see [this post](https://ro-che.info/articles/2014-04-24-lens-unidiomatic)).|] ],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let microlensItem = Item {
        _itemUid = "13",
        _itemName = "microlens",
        _itemGroup_ = Nothing,
        _itemPros = [
           Trait "131" $ T.unwords $ T.lines [text|
             Very small (the base package has no dependencies at all,
             and features like Template Haskell lens generation or
             instances for `Vector`/`Text`/`HashMap` are separated into
             other packages).|] ],
        _itemCons = [
           Trait "132" $ T.unwords $ T.lines [text|
             Doesn't provide lens's more advanced features (like prisms
             or indexed traversals).|],
           Trait "134" $ T.unwords $ T.lines [text|
             Doesn't let you write code in fully “lensy” style (since it
             omits lots of operators and `*Of` functions from lens).|] ],
        _itemLink = Just "https://github.com/aelve/microlens",
        _itemKind = hackageLibrary }
  let lensesCategory = Category {
        _categoryUid = "1",
        _categoryTitle = "Lenses",
        _categoryNotes = "Lenses are first-class composable accessors.",
        _categoryGroups = mempty,
        _categoryItems = [lensItem, microlensItem] }

  let parsecItem = Item {
        _itemUid = "21",
        _itemName = "parsec",
        _itemGroup_ = Just "parsec-like",
        _itemPros = [Trait "211" "the most widely used package",
                     Trait "213" "has lots of tutorials, book coverage, etc"],
        _itemCons = [Trait "212" "development has stagnated"],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let megaparsecItem = Item {
        _itemUid = "22",
        _itemName = "megaparsec",
        _itemGroup_ = Nothing,
        _itemPros = [Trait "221" "the API is largely similar to Parsec, \
                                 \so existing tutorials/code samples \
                                 \could be reused and migration is easy"],
        _itemCons = [],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let attoparsecItem = Item {
        _itemUid = "23",
        _itemName = "attoparsec",
        _itemGroup_ = Nothing,
        _itemPros = [Trait "231" "very fast, good for parsing binary formats"],
        _itemCons = [Trait "232" "can't report positions of parsing errors",
                     Trait "234" "doesn't provide a monad transformer"],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let parsingCategory = Category {
        _categoryUid = "2",
        _categoryTitle = "Parsing",
        _categoryNotes = "Parsers are parsers.",
        _categoryGroups = M.fromList [("parsec-like", Hue 1)],
        _categoryItems = [parsecItem, megaparsecItem, attoparsecItem] }

  -- As many different groups as there are different hues
  let def = Item {
        _itemUid = undefined,
        _itemName = undefined,
        _itemGroup_ = Nothing,
        _itemPros = [],
        _itemCons = [],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let item1 = def {
        _itemUid = "31",
        _itemName = "api-builder",
        _itemGroup_ = Just "group 1" }
  let item2 = def {
        _itemUid = "32",
        _itemName = "aeson",
        _itemGroup_ = Just "group 2" }
  let item3 = def {
        _itemUid = "33",
        _itemName = "unordered-containers",
        _itemGroup_ = Just "group 1" }
  let item4 = def {
        _itemUid = "34",
        _itemName = "lens",
        _itemGroup_ = Just "group 3" }
  let item5 = def {
        _itemUid = "35",
        _itemName = "bytestring",
        _itemGroup_ = Just "group 4" }
  let item6 = def {
        _itemUid = "36",
        _itemName = "microlens",
        _itemGroup_ = Nothing }
  let item7 = def {
        _itemUid = "37",
        _itemName = "parsec",
        _itemGroup_ = Nothing }
  let huesCategory = Category {
        _categoryUid = "3",
        _categoryTitle = "Testing hues",
        _categoryNotes = "Hopefully they all look good.",
        _categoryGroups =
           M.fromList [("group " <> tshow i, Hue i) | i <- [1..4]],
        _categoryItems = [item1, item2, item3, item4, item5, item6, item7] }

  GlobalState {_categories = [lensesCategory, parsingCategory, huesCategory]}

itemVar :: Path '[Uid]
itemVar = "item" <//> var

categoryVar :: Path '[Uid]
categoryVar = "category" <//> var

traitVar :: Path '[Uid]
traitVar = "trait" <//> var

withGlobal :: (MonadIO m, HasSpock m, SpockState m ~ IORef GlobalState)
           => State GlobalState a -> m a
withGlobal act = do
  stateVar <- Spock.getState
  liftIO $ atomicModifyIORef' stateVar (swap . runState act)

renderMethods :: SpockM () () (IORef GlobalState) ()
renderMethods = Spock.subcomponent "render" $ do
  -- Help
  Spock.get "help" $ do
    visible <- param' "mode"
    lucid $ renderHelp visible
  -- Title of a category
  Spock.get (categoryVar <//> "title") $ \catId -> do
    category <- withGlobal $ use (categoryById catId)
    renderMode <- param' "mode"
    lucid $ renderCategoryTitle renderMode category
  -- Notes for a category
  Spock.get (categoryVar <//> "notes") $ \catId -> do
    category <- withGlobal $ use (categoryById catId)
    renderMode <- param' "mode"
    lucid $ renderCategoryNotes renderMode category
  -- Item colors
  Spock.get (itemVar <//> "colors") $ \itemId -> do
    item <- withGlobal $ use (itemById itemId)
    category <- withGlobal $ use (categoryByItem itemId)
    let hue = getItemHue category item
    json $ M.fromList [("light" :: Text, hueToLightColor hue),
                       ("dark" :: Text, hueToDarkColor hue)]
  -- Item info
  Spock.get (itemVar <//> "info") $ \itemId -> do
    item <- withGlobal $ use (itemById itemId)
    renderMode <- param' "mode"
    cat <- withGlobal $ use (categoryByItem itemId)
    lucid $ renderItemInfo renderMode cat item
  -- All item traits
  Spock.get (itemVar <//> "traits") $ \itemId -> do
    item <- withGlobal $ use (itemById itemId)
    renderMode <- param' "mode"
    cat <- withGlobal $ use (categoryByItem itemId)
    lucid $ renderItemTraits renderMode cat item
  -- A single trait
  Spock.get (itemVar <//> traitVar) $ \itemId traitId -> do
    trait <- withGlobal $ use (itemById itemId . traitById traitId)
    renderMode <- param' "mode"
    lucid $ renderTrait renderMode itemId trait

setMethods :: SpockM () () (IORef GlobalState) ()
setMethods = Spock.subcomponent "set" $ do
  -- Title of a category
  Spock.post (categoryVar <//> "title") $ \catId -> do
    content' <- param' "content"
    changedCategory <- withGlobal $ do
      categoryById catId . title .= content'
      use (categoryById catId)
    lucid $ renderCategoryTitle Editable changedCategory
  -- Notes for a category
  Spock.post (categoryVar <//> "notes") $ \catId -> do
    content' <- param' "content"
    changedCategory <- withGlobal $ do
      categoryById catId . notes .= content'
      use (categoryById catId)
    lucid $ renderCategoryNotes Editable changedCategory
  -- Item info
  Spock.post (itemVar <//> "info") $ \itemId -> do
    -- TODO: rename to “itemLens” and “categoryLens”?
    let category :: Lens' GlobalState Category
        category = categoryByItem itemId
    let item :: Lens' GlobalState Item
        item = itemById itemId
    -- TODO: add a jumpy note saying where the form is handled
    -- and other notes saying where stuff is rendered, etc
    name' <- T.strip <$> param' "name"
    link' <- T.strip <$> param' "link"
    onHackage' <- (== Just ("on" :: Text)) <$> param "on-hackage"
    group' <- do
      groupField <- param' "group"
      customGroupField <- param' "custom-group"
      if | groupField == "-"           -> return Nothing
         | groupField == newGroupValue -> return (Just customGroupField)
         | otherwise                   -> return (Just groupField)
    -- If the group is a new one (entered in the “custom group” field), add
    -- it to the list of groups in the category (which would cause a new hue
    -- to be generated)
    case group' of
      Nothing -> return ()
      Just x  -> withGlobal $ category.groups %= addGroupIfDoesNotExist x
    -- Update list of groups if the group removed was the last of its kind.
    -- Note that this should be done after adding a new group because we also
    -- want the color to change. If the item was the only item in its group,
    -- then the sequence of actions would be as follows:
    -- 
    --   * new group is added (and hence new color is assigned)
    --   * old group is deleted (and now the old color is unused)
    withGlobal $ do
      oldGroup <- use (item.group_)
      case oldGroup of
        Nothing -> return ()
        Just g  -> when (oldGroup /= group') $ do
          allItems <- use (category.items)
          let inSameGroup item' = item'^.group_ == Just g
              isUnique = length (filter inSameGroup allItems) == 1
          when isUnique $
            category.groups %= M.delete g
    -- Modify the item
    changedItem <- withGlobal $ do
      -- TODO: actually validate the form and report errors
      unless (T.null name') $
        item.name .= name'
      case (T.null link', sanitiseUrl link') of
        (True, _)   -> item.link .= Nothing
        (_, Just l) -> item.link .= Just l
        _otherwise  -> return ()
      item.kind.onHackage .= onHackage'
      item.group_ .= group'
      use item
    cat <- withGlobal $ use category
    lucid $ renderItemInfo Editable cat changedItem
  -- Trait
  Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
    content' <- param' "content"
    changedTrait <- withGlobal $ do
      itemById itemId . traitById traitId . content .= content'
      use (itemById itemId . traitById traitId)
    lucid $ renderTrait Editable itemId changedTrait

addMethods :: SpockM () () (IORef GlobalState) ()
addMethods = Spock.subcomponent "add" $ do
  -- New category
  Spock.post "category" $ do
    content' <- param' "content"
    uid' <- randomUid
    let newCategory = Category {
          _categoryUid = uid',
          _categoryTitle = content',
          _categoryNotes = "(write some notes here, describe the category, etc)",
          _categoryGroups = mempty,
          _categoryItems = [] }
    withGlobal $ categories %= (newCategory :)
    lucid $ renderCategory newCategory
  -- New library in a category
  Spock.post (categoryVar <//> "library") $ \catId -> do
    itemName <- param' "name"
    itemId <- randomUid
    let newItem = Item {
          _itemUid    = itemId,
          _itemName   = itemName,
          _itemGroup_ = Nothing,
          _itemPros   = [],
          _itemCons   = [],
          _itemLink   = Nothing,
          _itemKind   = hackageLibrary }
    -- TODO: maybe do something if the category doesn't exist (e.g. has been
    -- already deleted)
    withGlobal $ categoryById catId . items %= (++ [newItem])
    cat <- withGlobal $ use (categoryByItem itemId)
    lucid $ renderItem Editable cat newItem
  -- Pro (argument in favor of a library)
  Spock.post (itemVar <//> "pro") $ \itemId -> do
    content' <- param' "content"
    uid' <- randomUid
    let newTrait = Trait uid' content'
    withGlobal $ itemById itemId . pros %= (++ [newTrait])
    lucid $ renderTrait Editable itemId newTrait
  -- Con (argument against a library)
  Spock.post (itemVar <//> "con") $ \itemId -> do
    content' <- param' "content"
    uid' <- randomUid
    let newTrait = Trait uid' content'
    withGlobal $ itemById itemId . cons %= (++ [newTrait])
    lucid $ renderTrait Editable itemId newTrait

otherMethods :: SpockM () () (IORef GlobalState) ()
otherMethods = do
  -- Search
  Spock.post "search" $ do
    query <- param' "query"
    let queryWords = T.words query
    let rank :: Category -> Int
        rank cat = sum [
          length (queryWords `intersect` (cat^..items.each.name)),
          length (queryWords `intersect` T.words (cat^.title)) ]
    cats <- withGlobal (use categories)
    let rankedCats
          | null queryWords = cats
          | otherwise       = filter ((/= 0) . rank) .
                              reverse . sortOn rank $ cats
    lucid $ renderCategoryList rankedCats

  -- Moving things
  Spock.subcomponent "move" $ do
    -- Move trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      direction :: Text <- param' "direction"
      let move = if direction == "up" then moveUp else moveDown
      withGlobal $ do
        itemById itemId . pros %= move ((== traitId) . view uid)
        itemById itemId . cons %= move ((== traitId) . view uid)
    -- Move item
    Spock.post itemVar $ \itemId -> do
      direction :: Text <- param' "direction"
      let move = if direction == "up" then moveUp else moveDown
      withGlobal $ do
        categoryByItem itemId . items %= move ((== itemId) . view uid)

  -- Deleting things
  Spock.subcomponent "delete" $ do
    -- Delete trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      withGlobal $ do
        itemById itemId . pros %= filter ((/= traitId) . view uid)
        itemById itemId . cons %= filter ((/= traitId) . view uid)
    -- Delete item
    Spock.post itemVar $ \itemId -> do
      let category :: Lens' GlobalState Category
          category = categoryByItem itemId
      let item :: Lens' GlobalState Item
          item = itemById itemId
      -- If the item was the only item in its group, delete the group (and
      -- make the hue available for new items)
      withGlobal $ do
        oldGroup <- use (item.group_)
        case oldGroup of
          Nothing -> return ()
          Just g  -> do
            allItems <- use (category.items)
            let inSameGroup item' = item'^.group_ == Just g
                isUnique = length (filter inSameGroup allItems) == 1
            when isUnique $
              category.groups %= M.delete g
      -- Delete the item
      withGlobal $ do
        category.items %= filter ((/= itemId) . view uid)

main :: IO ()
main = do
  stateVar <- newIORef sampleState
  let config = defaultSpockCfg () PCNoDatabase stateVar
  runSpock 8080 $ spock config $ do
    middleware (staticPolicy (addBase "static"))
    -- Main page
    Spock.get root $ do
      s <- liftIO $ readIORef stateVar
      lucid $ renderRoot s
    -- The add/set methods return rendered parts of the structure (added
    -- categories, changed items, etc) so that the Javascript part could take
    -- them and inject into the page. We don't want to duplicate rendering on
    -- server side and on client side.
    renderMethods
    setMethods
    addMethods
    otherMethods

renderRoot :: GlobalState -> HtmlT IO ()
renderRoot globalState = do
  includeJS "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js"
  includeCSS "/css.css"
  -- Include definitions of all Javascript functions that we have defined in
  -- this file.
  script_ (fromJS allJSFunctions)
  h1_ "Collaborative notes on Haskell libraries and tools"
  -- By default help is rendered hidden, and then showOrHideHelp reads a
  -- value from local storage and decides whether to show help or not. On one
  -- hand, it means that people with Javascript turned off won't be able to
  -- see help; on another hand, those people don't need help anyway because
  -- they won't be able to edit anything either.
  renderHelp Hidden
  onPageLoad $ JS.showOrHideHelp ("#help" :: JQuerySelector, helpVersion)
  -- TODO: use ordinary form-post search instead of Javascript search (for
  -- people with NoScript)
  textInput [
    id_ "search",
    placeholder_ "search",
    -- TODO: add something to construct a selector
    onEnter $ JS.search ("#categories" :: JQuerySelector, inputValue) ]
  textInput [
    placeholder_ "add a category",
    onEnter $ JS.addCategory ("#categories" :: JQuerySelector, inputValue) <>
              clearInput ]
  -- TODO: sort categories by popularity, somehow? or provide a list of
  -- “commonly used categories” or even a nested catalog
  renderCategoryList (globalState^.categories)
  -- TODO: perhaps use infinite scrolling/loading?
  -- TODO: add links to source and donation buttons
  -- TODO: add Piwik/Google Analytics
  -- TODO: maybe add a button like “give me random category that is unfinished”
  -- TODO: add CSS for blocks of code

-- Don't forget to change helpVersion when the text changes substantially
-- and you think the users should reread it.
helpVersion :: Int
helpVersion = 1

renderHelp :: Visible -> HtmlT IO ()
renderHelp Hidden =
  div_ [id_ "help"] $
    textButton "show help" $
      JS.showHelp ("#help" :: JQuerySelector, helpVersion)
renderHelp Shown =
  div_ [id_ "help"] $ do
    textButton "hide help" $
      JS.hideHelp ("#help" :: JQuerySelector, helpVersion)
    renderMarkdownBlock [text|
      You can edit everything, without registration. (But if you delete
      everything, I'll roll it back and then make a voodoo doll of you
      and stick some needles into it).
  
      The most important rule is: **it's collaborative notes, not Wikipedia**.
      In other words, incomplete entries like this are welcome here:
  
      > **pros:** pretty nice API\
      > **cons:** buggy (see an example on my Github, here's the link)
  
      Some additional guidelines/observations/etc that probably make sense:
  
        * sort pros/cons by importance
  
        * if you don't like something for any reason, edit it
  
        * if you're unsure about something, still write it
          (just warn others that you're unsure)
  
        * if you have useful information of any kind that doesn't fit,
          add it to the category notes
      |]

renderCategoryList :: [Category] -> HtmlT IO ()
renderCategoryList cats =
  div_ [id_ "categories"] $
    mapM_ renderCategory cats

renderCategoryTitle :: Editable -> Category -> HtmlT IO ()
renderCategoryTitle editable category =
  h2_ $ do
    a_ [class_ "anchor", href_ ("#" <> uidToText (category^.uid))] "#"
    titleNode <- thisNode
    case editable of
      Editable -> do
        toHtml (category^.title)
        emptySpan "1em"
        textButton "edit" $
          JS.setCategoryTitleMode (titleNode, category^.uid, InEdit)
      InEdit -> do
        textInput [
          value_ (category^.title),
          onEnter $
            JS.submitCategoryTitle (titleNode, category^.uid, inputValue) <>
            clearInput ]
        emptySpan "1em"
        textButton "cancel" $
          JS.setCategoryTitleMode (titleNode, category^.uid, Editable)

renderCategoryNotes :: Editable -> Category -> HtmlT IO ()
renderCategoryNotes editable category =
  div_ $ do
    this <- thisNode
    case editable of
      Editable -> do
        -- TODO: use shortcut-links
        renderMarkdownBlock (category^.notes)
        textButton "edit description" $
          JS.setCategoryNotesMode (this, category^.uid, InEdit)
      InEdit -> do
        textareaId <- randomUid
        textarea_ [uid_ textareaId,
                   rows_ "10", style_ "width:100%;resize:vertical"] $
          toHtml (category^.notes)
        button "Save" [] $ do
          -- «$("#<textareaId>").val()» is a Javascript expression that
          -- returns text contained in the textarea
          let textareaValue = JS $ format "$(\"#{}\").val()" [textareaId]
          JS.submitCategoryNotes (this, category^.uid, textareaValue)
        emptySpan "6px"
        button "Cancel" [] $
          JS.setCategoryNotesMode (this, category^.uid, Editable)
        emptySpan "6px"
        "Markdown"

renderCategory :: Category -> HtmlT IO ()
renderCategory category =
  div_ [class_ "category", uid_ (category^.uid)] $ do
    renderCategoryTitle Editable category
    renderCategoryNotes Editable category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ (renderItem Normal category) (category^.items)
      thisNode
    textInput [
      placeholder_ "add an item",
      onEnter $ JS.addLibrary (itemsNode, category^.uid, inputValue) <>
                clearInput ]

-- TODO: add arrows for moving items up and down in category, and something
-- to delete an item – those things could be at the left side, like on Reddit

getItemHue :: Category -> Item -> Hue
getItemHue category item = case item^.group_ of
  Nothing -> NoHue
  Just s  -> M.findWithDefault NoHue s (category^.groups)

-- TODO: perhaps use jQuery Touch Punch or something to allow dragging items
-- instead of using arrows? Touch Punch works on mobile, too
renderItem :: Editable -> Category -> Item -> HtmlT IO ()
renderItem editable cat item =
  div_ [class_ "item"] $ do
    itemNode <- thisNode
    -- TODO: the controls and item-info should be aligned (currently the
    -- controls are smaller)
    -- TODO: the controls should be “outside” of the main body width
    -- TODO: styles for all this should be in css.css
    div_ [class_ "item-controls"] $ do
      imgButton "/arrow-thick-top.svg" [width_ "12px",
                                        style_ "margin-bottom:5px"] $
        -- TODO: the item should blink or somehow else show where it has been
        -- moved
        JS.moveItemUp (item^.uid, itemNode)
      imgButton "/arrow-thick-bottom.svg" [width_ "12px",
                                           style_ "margin-bottom:5px"] $
        JS.moveItemDown (item^.uid, itemNode)
      imgButton "/x.svg" [width_ "12px"] $
        JS.deleteItem (item^.uid, itemNode, item^.name)
    -- This div is needed for “display:flex” on the outer div to work (which
    -- makes item-controls be placed to the left of everything else)
    div_ [style_ "width:100%"] $ do
      renderItemInfo Editable cat item
      case editable of
        Normal -> do
          renderItemTraits Normal cat item
        Editable -> do
          renderItemTraits Editable cat item

-- TODO: find some way to give all functions access to category and item (or
-- category, item and trait) without passing everything explicitly?

-- TODO: warn when a library isn't on Hackage but is supposed to be
-- TODO: give a link to oldest available docs when the new docs aren't there
renderItemInfo :: Editable -> Category -> Item -> HtmlT IO ()
renderItemInfo editable cat item = do
  let bg = hueToDarkColor $ getItemHue cat item
  div_ [class_ "item-info", style_ ("background-color:" <> bg)] $ do
    infoNode <- thisNode
    case editable of
      Editable -> do
        span_ [style_ "font-size:150%"] $ do
          -- If the library is on Hackage, the title links to its Hackage
          -- page; otherwise, it doesn't link anywhere. Even if the link
          -- field is present, it's going to be rendered as “(site)”, not
          -- linked in the title.
          let hackageLink = "https://hackage.haskell.org/package/" <>
                            item^.name
          case item^?kind.onHackage of
            Just True  -> a_ [href_ hackageLink] (toHtml (item^.name))
            _otherwise -> toHtml (item^.name)
          case item^.link of
            Just l  -> " (" >> a_ [href_ l] "site" >> ")"
            Nothing -> return ()
        emptySpan "2em"
        toHtml (fromMaybe "other" (item^.group_))
        emptySpan "2em"
        textButton "edit details" $
          JS.setItemInfoMode (infoNode, item^.uid, InEdit)
        -- TODO: link to Stackage too
        -- TODO: should check for Stackage automatically
      InEdit -> do
        let traitsNode = format ":has(> {}) > .item-traits" [infoNode]
        let formSubmitHandler formNode =
              JS.submitItemInfo (infoNode, traitsNode, item^.uid, formNode)
        form_ [onFormSubmit formSubmitHandler] $ do
          label_ $ do
            "Package name"
            br_ []
            input_ [type_ "text", name_ "name",
                    value_ (item^.name)]
          br_ []
          label_ $ do
            "Link to Hackage: "
            input_ $ [type_ "checkbox", name_ "on-hackage"] ++
                     [checked_ | item^?kind.onHackage == Just True]
          br_ []
          label_ $ do
            "Site (optional)"
            br_ []
            input_ [type_ "text", name_ "link",
                    value_ (fromMaybe "" (item^.link))]
          br_ []
          label_ $ do
            "Group"
            br_ []
            customInputId <- randomUid
            let selectHandler = [text|
                  if (this.value == "$newGroupValue") {
                    $("#$idText").show();
                    $("#$idText").focus(); }
                  else $("#$idText").hide(); |]
                  where idText = uidToText customInputId
            select_ [name_ "group", onchange_ selectHandler] $ do
              let gs = Nothing : map Just (M.keys (cat^.groups))
              for_ gs $ \group' -> do
                -- Text that will be shown in the list (“-” stands for “no
                -- group”)
                let txt = fromMaybe "-" group'
                -- If the element corresponds to the current group of the
                -- item (or the element is “-”, i.e. Nothing, and the group
                -- is Nothing too), mark it as selected, thus making it the
                -- element that will be chosen by default when the form is
                -- rendered
                if group' == item^.group_
                  then option_ [selected_ "selected", value_ txt] (toHtml txt)
                  else option_ [value_ txt] (toHtml txt)
              option_ [value_ newGroupValue] "New group..."
            input_ [uid_ customInputId, type_ "text",
                    name_ "custom-group", hidden_ "hidden"]
          br_ []
          input_ [type_ "submit", value_ "Save"]
          button "Cancel" [] $
            JS.setItemInfoMode (infoNode, item^.uid, Editable)

-- TODO: categories that don't directly compare libraries but just list all
-- libraries about something (e.g. Yesod plugins, or whatever)

-- TODO: categories without items (e.g. “web dev”) that list links to other
-- categories

renderItemTraits :: Editable -> Category -> Item -> HtmlT IO ()
renderItemTraits editable cat item = do
  let bg = hueToLightColor $ getItemHue cat item
  -- If the structure of HTML changes here, don't forget to update the
  -- 'traitsNode' selector in 'renderItemInfo'.
  div_ [class_ "item-traits", style_ ("background-color:" <> bg)] $ do
    this <- thisNode
    div_ [class_ "traits-groups-container"] $ do
      div_ [class_ "traits-group"] $ do
        p_ "Pros:"
        case editable of
          Normal ->
            ul_ $ mapM_ (renderTrait Normal (item^.uid)) (item^.pros)
          Editable -> do
            listNode <- ul_ $ do
              mapM_ (renderTrait Editable (item^.uid)) (item^.pros)
              thisNode
            textInput [
              placeholder_ "add pro",
              onEnter $ JS.addPro (listNode, item^.uid, inputValue) <>
                        clearInput ]
      -- TODO: maybe add a separator explicitly? instead of CSS
      div_ [class_ "traits-group"] $ do
        p_ "Cons:"
        -- TODO: maybe add a line here?
        case editable of
          Normal ->
            ul_ $ mapM_ (renderTrait Normal (item^.uid)) (item^.cons)
          Editable -> do
            listNode <- ul_ $ do
              mapM_ (renderTrait Editable (item^.uid)) (item^.cons)
              thisNode
            textInput [
              placeholder_ "add con",
              onEnter $ JS.addCon (listNode, item^.uid, inputValue) <>
                        clearInput ]
    case editable of
      Normal -> textButton "edit pros/cons" $
        JS.setItemTraitsMode (this, item^.uid, Editable)
      Editable -> textButton "edit off" $
        JS.setItemTraitsMode (this, item^.uid, Normal)

renderTrait :: Editable -> Uid -> Trait -> HtmlT IO ()
-- TODO: probably use renderMarkdownBlock here as well
renderTrait Normal _itemId trait = li_ (renderMarkdownLine (trait^.content))
renderTrait Editable itemId trait = li_ $ do
  this <- thisNode
  renderMarkdownLine (trait^.content)
  br_ []
  imgButton "/arrow-thick-top.svg" [width_ "12px"] $
    JS.moveTraitUp (itemId, trait^.uid, this)
  imgButton "/arrow-thick-bottom.svg" [width_ "12px"] $
    JS.moveTraitDown (itemId, trait^.uid, this)
  -- TODO: these 3 icons in a row don't look nice
  -- TODO: there should be some way to undelete things (e.g. a list of
  -- deleted traits under each item)
  imgButton "/x.svg" [width_ "12px"] $
    JS.deleteTrait (itemId, trait^.uid, this, trait^.content)
  textButton "edit" $
    JS.setTraitMode (this, itemId, trait^.uid, InEdit)
-- TODO: the text area should be bigger
renderTrait InEdit itemId trait = li_ $ do
  this <- thisNode
  let submitHandler = JS.submitTrait (this, itemId, trait^.uid, inputValue) <>
                      clearInput
  textarea_ [onEnter submitHandler] $ toHtml (trait^.content)
  br_ []
  textButton "cancel" $
    JS.setTraitMode (this, itemId, trait^.uid, Editable)

-- Utils

onPageLoad :: JS -> HtmlT IO ()
onPageLoad js = script_ $ format "$(document).ready(function(){{}});" [js]

emptySpan :: Text -> HtmlT IO ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

-- Use inputValue to get the value (works with input_ and textarea_)
onEnter :: JS -> Attribute
onEnter handler = onkeydown_ $ format "if (event.keyCode == 13) {{}}" [handler]

textInput :: [Attribute] -> HtmlT IO ()
textInput attrs = input_ (type_ "text" : attrs)

inputValue :: JS
inputValue = JS "this.value"

clearInput :: JS
clearInput = JS "this.value = '';"

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

button :: Text -> [Attribute] -> JS -> HtmlT IO ()
button value attrs handler =
  input_ (type_ "button" : value_ value : onclick_ handler' : attrs)
  where
    handler' = fromJS handler

-- A text button looks like “[cancel]”
-- 
-- TODO: consider dotted links instead?
textButton
  :: Text         -- ^ Button text
  -> JS           -- ^ Onclick handler
  -> HtmlT IO ()
textButton caption (JS handler) =
  span_ [class_ "text-button"] $
    a_ [href_ "javascript:void(0)", onclick_ handler] (toHtml caption)

-- So far all icons used here have been from <https://useiconic.com/open/>
imgButton :: Url -> [Attribute] -> JS -> HtmlT IO ()
imgButton src attrs (JS handler) =
  a_ [href_ "javascript:void(0)", onclick_ handler] (img_ (src_ src : attrs))

uid_ :: Uid -> Attribute
uid_ = id_ . uidToText

-- TODO: make this a newtype
type JQuerySelector = Text

thisNode :: HtmlT IO JQuerySelector
thisNode = do
  uid' <- randomUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [uid_ uid', class_ "dummy"] mempty
  return (format ":has(> #{})" [uid'])

data Editable = Normal | Editable | InEdit

instance PathPiece Editable where
  fromPathPiece "normal"   = Just Normal
  fromPathPiece "editable" = Just Editable
  fromPathPiece "in-edit"  = Just InEdit
  fromPathPiece _          = Nothing
  toPathPiece Normal   = "normal"
  toPathPiece Editable = "editable"
  toPathPiece InEdit   = "in-edit"

instance ToJS Editable where
  toJS = JS . tshow . toPathPiece

data Visible = Hidden | Shown

instance PathPiece Visible where
  fromPathPiece "hidden" = Just Hidden
  fromPathPiece "shown"  = Just Shown
  fromPathPiece _        = Nothing
  toPathPiece Hidden = "hidden"
  toPathPiece Shown  = "shown"

instance ToJS Visible where
  toJS = JS . tshow . toPathPiece

-- TODO: why not compare Haskellers too?

newGroupValue :: Text
newGroupValue = "-new-group-"
