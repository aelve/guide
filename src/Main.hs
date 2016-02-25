{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
RankNTypes,
FlexibleInstances,
FlexibleContexts,
QuasiQuotes,
ScopedTypeVariables,
FunctionalDependencies,
TypeFamilies,
DataKinds,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude hiding (Category)
-- Monads and monad transformers
import Control.Monad.State
-- Lenses
import Lens.Micro.Platform
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation
-- Randomness
import System.Random
-- Web
import Lucid hiding (for_)
import Web.Spock hiding (get, text)
import qualified Web.Spock as Spock
import Network.Wai.Middleware.Static
import Web.PathPieces

-- Local
import JS (JS(..), ToJS, allJSFunctions)
import qualified JS
import Utils


-- | Unique id, used for many things – categories, items, and anchor ids.
-- Note that in HTML 5 using numeric ids for divs, spans, etc is okay.
type Uid = Int

randomUid :: MonadIO m => m Uid
randomUid = liftIO $ randomRIO (0, 10^(9::Int))

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
  _itemUid  :: Uid,
  _itemName :: Text,
  _itemPros :: [Trait],
  _itemCons :: [Trait],
  _itemLink :: Maybe Url,
  _itemKind :: ItemKind }

makeFields ''Item

traitById :: Uid -> Lens' Item Trait
traitById uid' = singular $
  (pros.each . filtered ((== uid') . view uid)) `failing`
  (cons.each . filtered ((== uid') . view uid))

data Category = Category {
  _categoryUid :: Uid,
  _categoryTitle :: Text,
  _categoryNotes :: Text,
  _categoryItems :: [Item] }

makeFields ''Category

data GlobalState = GlobalState {
  _categories :: [Category] }

makeLenses ''GlobalState

categoryById :: Uid -> Lens' GlobalState Category
categoryById uid' = singular $
  categories.each . filtered ((== uid') . view uid)

itemById :: Uid -> Lens' GlobalState Item
itemById uid' = singular $
  categories.each . items.each . filtered ((== uid') . view uid)

emptyState :: GlobalState
emptyState = GlobalState {
  _categories = [] }

sampleState :: GlobalState
sampleState = do
  let lensItem = Item {
        _itemUid = 12,
        _itemName = "lens",
        _itemPros = [Trait 121 "the standard lenses library",
                     Trait 122 "batteries included"],
        _itemCons = [Trait 123 "huge"],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let microlensItem = Item {
        _itemUid = 13,
        _itemName = "microlens",
        _itemPros = [Trait 131 "very small",
                     Trait 132 "good for libraries"],
        _itemCons = [Trait 133 "doesn't have advanced features"],
        _itemLink = Just "https://github.com/aelve/microlens",
        _itemKind = hackageLibrary }
  let lensesCategory = Category {
        _categoryUid = 1,
        _categoryTitle = "Lenses",
        _categoryNotes = "Lenses are first-class composable accessors.",
        _categoryItems = [lensItem, microlensItem] }

  GlobalState {_categories = [lensesCategory]}

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
  -- Item info
  Spock.get (itemVar <//> "info") $ \itemId -> do
    item <- withGlobal $ use (itemById itemId)
    renderMode <- param' "mode"
    lucid $ renderItemInfo renderMode item
  -- All item traits
  Spock.get (itemVar <//> "traits") $ \itemId -> do
    item <- withGlobal $ use (itemById itemId)
    renderMode <- param' "mode"
    lucid $ renderItemTraits renderMode item
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
    name' <- T.strip <$> param' "name"
    link' <- T.strip <$> param' "link"
    onHackage' <- (== Just ("on" :: Text)) <$> param "on-hackage"
    changedItem <- withGlobal $ do
      let item :: Lens' GlobalState Item
          item = itemById itemId
      -- TODO: actually validate the form and report errors
      unless (T.null name') $
        item.name .= name'
      case (T.null link', sanitiseUrl link') of
        (True, _)   -> item.link .= Nothing
        (_, Just l) -> item.link .= Just l
        _otherwise  -> return ()
      item.kind.onHackage .= onHackage'
      use item
    lucid $ renderItemInfo Editable changedItem
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
          _categoryItems = [] }
    withGlobal $ categories %= (newCategory :)
    lucid $ renderCategory newCategory
  -- New library in a category
  Spock.post (categoryVar <//> "library") $ \catId -> do
    name' <- param' "name"
    uid' <- randomUid
    let newItem = Item {
          _itemUid  = uid',
          _itemName = name',
          _itemPros = [],
          _itemCons = [],
          _itemLink = Nothing,
          _itemKind = hackageLibrary }
    -- TODO: maybe do something if the category doesn't exist (e.g. has been
    -- already deleted)
    withGlobal $ categoryById catId . items %= (++ [newItem])
    lucid $ renderItem Editable newItem
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

  -- Deleting things
  Spock.subcomponent "delete" $ do
    -- Delete trait
    Spock.post (itemVar <//> traitVar) $ \itemId traitId -> do
      withGlobal $ do
        itemById itemId . pros %= filter ((/= traitId) . view uid)
        itemById itemId . cons %= filter ((/= traitId) . view uid)

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
  h1_ "A collaborative guide to Haskell libraries and tools"
  -- TODO: add a way to hide the rules
  div_ [id_ "help"] $ renderMarkdownBlock [text|
    You can edit everything, without registration. (But if you delete
    everything, I'll roll it back and then make a voodoo doll of you
    and stick some needles into it).

    Here are some guidelines/observations/etc that probably make sense:

      * sort pros/cons by importance
      * if you don't like something for any reason, edit it
      * if you're unsure about something, still write it
        (just warn others that you're unsure)
      * if you have useful information of any kind that doesn't fit,
        add it to the category notes

    Subjective judgements and incomplete entries are completely alright –
    it's not Wikipedia, it's collaborative notes, so don't be afraid to
    write anything here. Also, use bold/italics/code/links freely
    (Markdown is supported).
    |]
  textInput [id_ "search", placeholder_ "search"] $
    JS.search ("#categories" :: Text, inputValue)
  textInput [placeholder_ "add a category"] $
    JS.addCategory ("#categories" :: Text, inputValue) <> clearInput
  renderCategoryList (globalState^.categories)

renderCategoryList :: [Category] -> HtmlT IO ()
renderCategoryList cats =
  div_ [id_ "categories"] $
    mapM_ renderCategory cats

renderCategoryTitle :: Editable -> Category -> HtmlT IO ()
renderCategoryTitle editable category =
  h2_ $ do
    a_ [class_ "anchor", href_ ("#" <> tshow (category^.uid))] "#"
    titleNode <- thisNode
    case editable of
      Editable -> do
        toHtml (category^.title)
        emptySpan "1em"
        textButton "edit" $
          JS.setCategoryTitleMode (titleNode, category^.uid, InEdit)
      InEdit -> do
        textInput [value_ (category^.title)] $
          JS.submitCategoryTitle (titleNode, category^.uid, inputValue) <>
          clearInput
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
        textInput [value_ (category^.notes)] $
          JS.submitCategoryNotes (this, category^.uid, inputValue) <>
          clearInput
        textButton "cancel" $
          JS.setCategoryNotesMode (this, category^.uid, Editable)

renderCategory :: Category -> HtmlT IO ()
renderCategory category =
  div_ [class_ "category", id_ (tshow (category^.uid))] $ do
    renderCategoryTitle Editable category
    renderCategoryNotes Editable category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ (renderItem Normal) (category^.items)
      thisNode
    textInput [placeholder_ "add an item"] $
      JS.addLibrary (itemsNode, category^.uid, inputValue) <> clearInput

-- TODO: add arrows for moving items left-and-right in the category (or sort
-- them by popularity?)

renderItem :: Editable -> Item -> HtmlT IO ()
renderItem editable item =
  div_ [class_ "item"] $ do
    case editable of
      Normal -> do
        renderItemInfo Editable item
        renderItemTraits Normal item
      Editable -> do
        renderItemInfo Editable item
        renderItemTraits Editable item

-- TODO: warn when a library isn't on Hackage but is supposed to be
-- TODO: give a link to oldest available docs when the new docs aren't there
renderItemInfo :: Editable -> Item -> HtmlT IO ()
renderItemInfo editable item =
  div_ $ do
    this <- thisNode
    case editable of
      Editable -> h3_ $ do
        -- If the library is on Hackage, the title links to its Hackage page;
        -- otherwise, it doesn't link anywhere. Even if the link field is
        -- present, it's going to be rendered as “(site)”, not linked in the
        -- title.
        let hackageLink = "https://hackage.haskell.org/package/" <> item^.name
        case item^?kind.onHackage of
          Just True  -> a_ [href_ hackageLink] (toHtml (item^.name))
          _otherwise -> toHtml (item^.name)
        case item^.link of
          Just l  -> " (" >> a_ [href_ l] "site" >> ")"
          Nothing -> return ()
        emptySpan "1em"
        textButton "edit" $
          JS.setItemInfoMode (this, item^.uid, InEdit)
      InEdit -> do
        let handler s = JS.submitItemInfo (this, item^.uid, s)
        form_ [onFormSubmit handler] $ do
          label_ $ do
            "Package name: "
            input_ [type_ "text", name_ "name",
                    value_ (item^.name)]
          br_ []
          label_ $ do
            "On Hackage: "
            input_ $ [type_ "checkbox", name_ "on-hackage"] ++
                     [checked_ | item^?kind.onHackage == Just True]
          br_ []
          label_ $ do
            "Site (optional): "
            input_ [type_ "text", name_ "link",
                    value_ (fromMaybe "" (item^.link))]
          br_ []
          input_ [type_ "submit", value_ "Submit"]
          button "Cancel" [] $
            JS.setItemInfoMode (this, item^.uid, Editable)

-- TODO: categories that don't directly compare libraries but just list all
-- libraries about something (e.g. Yesod plugins, or whatever)

renderItemTraits :: Editable -> Item -> HtmlT IO ()
renderItemTraits editable item =
  div_ [class_ "traits"] $ do
    this <- thisNode
    case editable of
      Normal -> textButton "edit" $
        JS.setItemTraitsMode (this, item^.uid, Editable)
      Editable -> textButton "edit off" $
        JS.setItemTraitsMode (this, item^.uid, Normal)
    div_ [class_ "traits-group"] $ do
      p_ "Pros:"
      case editable of
        Normal ->
          ul_ $ mapM_ (renderTrait Normal (item^.uid)) (item^.pros)
        Editable -> do
          listNode <- ul_ $ do
            mapM_ (renderTrait Editable (item^.uid)) (item^.pros)
            thisNode
          textInput [placeholder_ "add pro"] $
            JS.addPro (listNode, item^.uid, inputValue) <> clearInput
    div_ [class_ "traits-group"] $ do
      p_ "Cons:"
      case editable of
        Normal ->
          ul_ $ mapM_ (renderTrait Normal (item^.uid)) (item^.cons)
        Editable -> do
          listNode <- ul_ $ do
            mapM_ (renderTrait Editable (item^.uid)) (item^.cons)
            thisNode
          textInput [placeholder_ "add con"] $
            JS.addCon (listNode, item^.uid, inputValue) <> clearInput

renderTrait :: Editable -> Uid -> Trait -> HtmlT IO ()
renderTrait Normal _itemId trait = li_ (renderMarkdownLine (trait^.content))
renderTrait Editable itemId trait = li_ $ do
  this <- thisNode
  renderMarkdownLine (trait^.content)
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
renderTrait InEdit itemId trait = li_ $ do
  this <- thisNode
  textInput [value_ (trait^.content)] $
    JS.submitTrait (this, itemId, trait^.uid, inputValue) <> clearInput
  textButton "cancel" $
    JS.setTraitMode (this, itemId, trait^.uid, Editable)

-- Utils

emptySpan :: Text -> HtmlT IO ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

textInput :: [Attribute] -> JS -> HtmlT IO ()
textInput attrs handler =
  input_ (type_ "text" : onkeyup_ handler' : attrs)
  where
    handler' = format "if (event.keyCode == 13) {{}}" [handler]

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

type JQuerySelector = Text

thisNode :: HtmlT IO JQuerySelector
thisNode = do
  uid' <- randomUid
  -- If the class name ever changes, fix 'JS.moveNodeUp' and
  -- 'JS.moveNodeDown'.
  span_ [id_ (tshow uid'), class_ "dummy"] mempty
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

-- TODO: why not compare Haskellers too?
