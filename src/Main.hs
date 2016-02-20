{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
RecordWildCards,
RankNTypes,
FlexibleInstances,
FlexibleContexts,
QuasiQuotes,
ScopedTypeVariables,
MultiParamTypeClasses,
FunctionalDependencies,
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
import qualified Data.Text.Lazy as TL
import Data.Text.Format hiding (format)
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
import NeatInterpolation
-- Randomness
import System.Random
-- Web
import Lucid hiding (for_)
import Web.Spock hiding (get, text)
import qualified Web.Spock as Spock
import Network.Wai.Middleware.Static
import Web.PathPieces


type Url = Text

-- | Unique id, used for many things – categories, items, and anchor ids.
-- Note that in HTML 5 using numeric ids for divs, spans, etc is okay.
type Uid = Int

randomUid :: MonadIO m => m Uid
randomUid = liftIO $ randomRIO (0, 10^(9::Int))

data Trait = Trait {
  _traitUid :: Uid,
  _traitContent :: Text }

makeFields ''Trait

data ItemKind = HackageLibrary | Library | Unknown

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
  _categoryDescription :: Text,
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
        _itemKind = HackageLibrary }
  let microlensItem = Item {
        _itemUid = 13,
        _itemName = "microlens",
        _itemPros = [Trait 131 "very small",
                     Trait 132 "good for libraries"],
        _itemCons = [Trait 133 "doesn't have advanced features"],
        _itemLink = Just "https://github.com/aelve/microlens",
        _itemKind = HackageLibrary }
  let lensesCategory = Category {
        _categoryUid = 1,
        _categoryTitle = "lenses",
        _categoryDescription = "Lenses are first-class composable accessors.",
        _categoryItems = [lensItem, microlensItem] }

  GlobalState {_categories = [lensesCategory]}

itemVar :: Path '[Uid]
itemVar = "item" <//> var

categoryVar :: Path '[Uid]
categoryVar = "category" <//> var

traitVar :: Path '[Uid]
traitVar = "trait" <//> var

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  middleware (staticPolicy (addBase "static"))
  stateVar <- liftIO $ newIORef sampleState
  let withGlobal :: MonadIO m => State GlobalState a -> m a
      withGlobal f = liftIO $ atomicModifyIORef' stateVar (swap . runState f)

  -- Main page
  Spock.get root $ do
    s <- liftIO $ readIORef stateVar
    lucid $ renderRoot s

  -- Render methods
  Spock.subcomponent "render" $ do
    -- Title of a category
    Spock.get (categoryVar <//> "title") $ \catId -> do
      category <- withGlobal $ use (categoryById catId)
      renderMode <- param' "mode"
      lucid $ renderCategoryTitle renderMode category
    -- Description of a category
    Spock.get (categoryVar <//> "description") $ \catId -> do
      category <- withGlobal $ use (categoryById catId)
      renderMode <- param' "mode"
      lucid $ renderCategoryDescription renderMode category
    -- Item
    Spock.get itemVar $ \itemId -> do
      item <- withGlobal $ use (itemById itemId)
      lucid $ renderItem item
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
    Spock.get (itemVar <//> traitVar) $
      \itemId traitId -> do
         trait <- withGlobal $ use (itemById itemId . traitById traitId)
         renderMode <- param' "mode"
         lucid $ renderTrait renderMode itemId trait

  -- The add/set methods return rendered parts of the structure (added
  -- categories, changed items, etc) so that the Javascript part could take
  -- them and inject into the page. We don't want to duplicate rendering on
  -- server side and on client side.

  -- Set methods
  Spock.subcomponent "set" $ do
    -- Title of a category
    Spock.post (categoryVar <//> "title") $ \catId -> do
      content' <- param' "content"
      changedCategory <- withGlobal $ do
        categoryById catId . title .= content'
        use (categoryById catId)
      lucid $ renderCategoryTitle Editable changedCategory
    -- Description of a category
    Spock.post (categoryVar <//> "description") $ \catId -> do
      content' <- param' "content"
      changedCategory <- withGlobal $ do
        categoryById catId . description .= content'
        use (categoryById catId)
      lucid $ renderCategoryDescription Editable changedCategory
    -- Trait
    Spock.post (itemVar <//> traitVar) $
      \itemId traitId -> do
         content' <- param' "content"
         changedTrait <- withGlobal $ do
           itemById itemId . traitById traitId . content .= content'
           use (itemById itemId . traitById traitId)
         lucid $ renderTrait Editable itemId changedTrait

  -- Add methods
  Spock.subcomponent "add" $ do
    -- New category
    Spock.post "category" $ do
      content' <- param' "content"
      uid' <- randomUid
      let newCategory = Category {
            _categoryUid = uid',
            _categoryTitle = content',
            _categoryDescription = "<write a description here>",
            _categoryItems = [] }
      withGlobal $ categories %= (++ [newCategory])
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
            _itemKind = HackageLibrary }
      -- TODO: maybe do something if the category doesn't exist (e.g. has been
      -- already deleted)
      withGlobal $ categoryById catId . items %= (++ [newItem])
      lucid $ renderItem newItem
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

renderRoot :: GlobalState -> HtmlT IO ()
renderRoot globalState = do
  includeJS "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js"
  includeCSS "/css.css"
  -- Include definitions of all Javascript functions that we have defined in
  -- this file.
  script_ $ T.unlines (map snd (allJSFunctions :: [(Text, JS)]))
  categoriesNode <- div_ [id_ "categories"] $ do
    mapM_ renderCategory (globalState ^. categories)
    thisNode
  let handler s = js_addCategory (categoriesNode, s)
  input_ [type_ "text", placeholder_ "new category", submitFunc handler]

renderCategoryTitle :: Editable -> Category -> HtmlT IO ()
renderCategoryTitle editable category =
  h2_ [id_ (tshow (category^.uid))] $ do
    a_ [class_ "anchor", href_ ("#" <> tshow (category^.uid))] "#"
    titleNode <- thisNode
    case editable of
      Editable -> do
        toHtml (category^.title)
        textButton "edit" $
          js_setCategoryTitleMode (titleNode, category^.uid, InEdit)
      InEdit -> do
        let handler s = js_submitCategoryTitleEdit
                          (titleNode, category^.uid, s)
        input_ [type_ "text", value_ (category^.title), submitFunc handler]
        textButton "cancel" $
          js_setCategoryTitleMode (titleNode, category^.uid, Editable)

-- TODO: render descriptions and traits as Markdown

renderCategoryDescription :: Editable -> Category -> HtmlT IO ()
renderCategoryDescription editable category =
  p_ $ do
    descrNode <- thisNode
    case editable of
      Editable -> do
        toHtml (category^.description)
        textButton "edit" $
          js_setCategoryDescriptionMode (descrNode, category^.uid, InEdit)
      InEdit -> do
        let handler s = js_submitCategoryDescriptionEdit
                          (descrNode, category^.uid, s)
        input_ [type_ "text", value_ (category^.description), submitFunc handler]
        textButton "cancel" $
          js_setCategoryDescriptionMode (descrNode, category^.uid, Editable)

renderCategory :: Category -> HtmlT IO ()
renderCategory category =
  div_ $ do
    renderCategoryTitle Editable category
    renderCategoryDescription Editable category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ renderItem (category^.items)
      thisNode
    let handler s = js_addLibrary (itemsNode, category^.uid, s)
    input_ [type_ "text", placeholder_ "new item", submitFunc handler]

renderItem :: Item -> HtmlT IO ()
renderItem item =
  div_ [class_ "item"] $ do
    renderItemInfo Normal item
    renderItemTraits Normal item

renderItemInfo :: Editable -> Item -> HtmlT IO ()
renderItemInfo editable item =
  h3_ $ do
    this <- thisNode
    -- If the library is on Hackage, the title links to its Hackage page;
    -- otherwise, it doesn't link anywhere. Even if the link field is
    -- present, it's going to be rendered as “(site)”, not linked in the
    -- title.
    case item^.kind of
      HackageLibrary -> a_ [href_ hackageLink] (toHtml (item^.name))
      _otherwise     -> toHtml (item^.name)
    case item^.link of
      Just l  -> " (" >> a_ [href_ l] "site" >> ")"
      Nothing -> return ()
    case editable of
      Normal -> textButton "edit" $
        js_setItemInfoMode (this, item^.uid, Editable)
      -- TODO: change to an actual button, “Submit”, etc.
      Editable -> textButton "edit off" $
        js_setItemInfoMode (this, item^.uid, Normal)
  where
    hackageLink = "https://hackage.haskell.org/package/" <> item^.name

renderItemTraits :: Editable -> Item -> HtmlT IO ()
renderItemTraits editable item =
  div_ [class_ "traits"] $ do
    this <- thisNode
    case editable of
      Normal -> textButton "edit" $
        js_setItemTraitsMode (this, item^.uid, Editable)
      Editable -> textButton "edit off" $
        js_setItemTraitsMode (this, item^.uid, Normal)
    div_ [class_ "traits-group"] $ do
      p_ "Pros:"
      case editable of
        Normal ->
          ul_ $ mapM_ (renderTrait Normal (item^.uid)) (item^.pros)
        Editable -> do
          listNode <- ul_ $ do
            mapM_ (renderTrait Editable (item^.uid)) (item^.pros)
            thisNode
          let handler s = js_addPro (listNode, item^.uid, s)
          input_ [type_ "text", placeholder_ "add pro", submitFunc handler]
    div_ [class_ "traits-group"] $ do
      p_ "Cons:"
      case editable of
        Normal ->
          ul_ $ mapM_ (renderTrait Normal (item^.uid)) (item^.cons)
        Editable -> do
          listNode <- ul_ $ do
            mapM_ (renderTrait Editable (item^.uid)) (item^.cons)
            thisNode
          let handler s = js_addCon (listNode, item^.uid, s)
          input_ [type_ "text", placeholder_ "add con", submitFunc handler]

renderTrait :: Editable -> Uid -> Trait -> HtmlT IO ()
renderTrait Normal _ trait = li_ (toHtml (trait^.content))
renderTrait Editable itemId trait = li_ $ do
  this <- thisNode
  toHtml (trait^.content)
  textButton "edit" $
    js_setTraitMode (this, itemId, trait^.uid, InEdit)
renderTrait InEdit itemId trait = li_ $ do
  this <- thisNode
  let handler s = js_submitTraitEdit
                    (this, itemId, trait^.uid, s)
  input_ [type_ "text", value_ (trait^.content), submitFunc handler]
  textButton "cancel" $
    js_setTraitMode (this, itemId, trait^.uid, Editable)

-- Utils

includeJS :: Monad m => Url -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Monad m => Url -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

-- The function is passed a JS expression that refers to text being submitted.
submitFunc :: (JS -> JS) -> Attribute
submitFunc f = onkeyup_ $ format
  "if (event.keyCode == 13) {\
  \  {}\
  \  this.value = ''; }"
  [f "this.value"]

-- Javascript

-- TODO: try to make them more type-safe somehow?

class JSFunction a where
  makeJSFunction
    :: Text          -- Name
    -> JS            -- Definition
    -> a

-- This generates function name
instance JSFunction Text where
  makeJSFunction fName _ = fName

-- This generates function definition and direct dependencies
instance JSFunction (Text, JS) where
  makeJSFunction fName fDef = (fName, fDef)

-- This generates a function that takes arguments and produces a Javascript
-- function call
instance JSParams a => JSFunction (a -> JS) where
  makeJSFunction fName _ = \args ->
    fName <> "(" <> T.intercalate "," (jsParams args) <> ");"

allJSFunctions :: JSFunction a => [a]
allJSFunctions = [
  -- Utilities
  js_replaceWithData, js_appendData,
  -- Add methods
  js_addLibrary, js_addCategory,
  js_addPro, js_addCon,
  -- Render-as-editable methods
  js_setCategoryTitleMode, js_setCategoryDescriptionMode,
  js_setItemInfoMode, js_setItemTraitsMode,
  js_setTraitMode,
  -- Set methods
  js_submitCategoryTitleEdit,
  js_submitTraitEdit,
  js_submitCategoryDescriptionEdit ]

js_replaceWithData :: JSFunction a => a
js_replaceWithData = makeJSFunction "replaceWithData" [text|
  function replaceWithData(node) {
    return function(data) {$(node).replaceWith(data); }; }
  |]

js_appendData :: JSFunction a => a
js_appendData = makeJSFunction "appendData" [text|
  function appendData(node) {
    return function(data) {$(node).append(data); }; }
  |]

-- | Create a new category.
js_addCategory :: JSFunction a => a
js_addCategory = makeJSFunction "addCategory" [text|
  function addCategory(node, s) {
    $.post("/add/category", {content: s})
     .done(appendData(node));
    }
  |]

-- | Add a new library to some category.
js_addLibrary :: JSFunction a => a
js_addLibrary = makeJSFunction "addLibrary" [text|
  function addLibrary(node, catId, s) {
    $.post("/add/category/"+catId+"/library", {name: s})
     .done(appendData(node));
    }
  |]

js_setCategoryTitleMode :: JSFunction a => a
js_setCategoryTitleMode = makeJSFunction "setCategoryTitleMode" [text|
  function setCategoryTitleMode(node, catId, mode) {
    $.get("/render/category/"+catId+"/title", {mode: mode})
     .done(replaceWithData(node));
    }
  |]

{- |
Finish category title editing (this happens when you submit the field).

This turns the title with the editbox back into a simple text title.
-}
js_submitCategoryTitleEdit :: JSFunction a => a
js_submitCategoryTitleEdit = makeJSFunction "submitCategoryTitleEdit" [text|
  function submitCategoryTitleEdit(node, catId, s) {
    $.post("/set/category/"+catId+"/title", {content: s})
     .done(replaceWithData(node));
    }
  |]

js_setCategoryDescriptionMode :: JSFunction a => a
js_setCategoryDescriptionMode = makeJSFunction "setCategoryDescriptionMode" [text|
  function setCategoryDescriptionMode(node, catId, mode) {
    $.get("/render/category/"+catId+"/description", {mode: mode})
     .done(replaceWithData(node));
    }
  |]

{- |
Finish category description editing (this happens when you submit the field).

This turns the description with the editbox back into a simple text description.
-}
js_submitCategoryDescriptionEdit :: JSFunction a => a
js_submitCategoryDescriptionEdit = makeJSFunction "submitCategoryDescriptionEdit" [text|
  function submitCategoryDescriptionEdit(node, catId, s) {
    $.post("/set/category/"+catId+"/description", {content: s})
     .done(replaceWithData(node));
    }
  |]

-- | Add a pro to some item.
js_addPro :: JSFunction a => a
js_addPro = makeJSFunction "addPro" [text|
  function addPro(node, itemId, s) {
    $.post("/add/item/"+itemId+"/pro", {content: s})
     .done(appendData(node));
    }
  |]

-- | Add a con to some item.
js_addCon :: JSFunction a => a
js_addCon = makeJSFunction "addCon" [text|
  function addCon(node, itemId, s) {
    $.post("/add/item/"+itemId+"/con", {content: s})
     .done(appendData(node));
    }
  |]

js_setItemInfoMode :: JSFunction a => a
js_setItemInfoMode = makeJSFunction "setItemInfoMode" [text|
  function setItemInfoMode(node, itemId, mode) {
    $.get("/render/item/"+itemId+"/info", {mode: mode})
     .done(replaceWithData(node));
    }
  |]

js_setItemTraitsMode :: JSFunction a => a
js_setItemTraitsMode = makeJSFunction "setItemTraitsMode" [text|
  function setItemTraitsMode(node, itemId, mode) {
    $.get("/render/item/"+itemId+"/traits", {mode: mode})
     .done(replaceWithData(node));
    }
  |]

js_setTraitMode :: JSFunction a => a
js_setTraitMode = makeJSFunction "setTraitMode" [text|
  function setTraitMode(node, itemId, traitId, mode) {
    $.get("/render/item/"+itemId+"/trait/"+traitId, {mode: mode})
     .done(replaceWithData(node));
    }
  |]

js_submitTraitEdit :: JSFunction a => a
js_submitTraitEdit = makeJSFunction "submitTraitEdit" [text|
  function submitTraitEdit(node, itemId, traitId, s) {
    $.post("/set/item/"+itemId+"/trait/"+traitId, {content: s})
     .done(replaceWithData(node));
    }
  |]

-- When adding a function, don't forget to add it to 'allJSFunctions'!

type JS = Text

-- A text button looks like “[cancel]”
textButton
  :: Text         -- ^ Button text
  -> JS           -- ^ Onclick handler
  -> HtmlT IO ()
textButton caption handler =
  span_ [class_ "textButton"] $
    a_ [href_ "javascript:void(0)", onclick_ handler] (toHtml caption)

type JQuerySelector = Text

thisNode :: HtmlT IO JQuerySelector
thisNode = do
  uid' <- randomUid
  span_ [id_ (tshow uid')] mempty
  return (T.pack (show (format ":has(> #{})" [uid'])))

lucid :: HtmlT IO a -> ActionT IO a
lucid h = do
  htmlText <- liftIO (renderTextT h)
  html (TL.toStrict htmlText)

-- | Format a string (a bit 'Text.Printf.printf' but with different syntax).
format :: Format.Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)

tshow :: Show a => a -> Text
tshow = T.pack . show

data Editable = Normal | Editable | InEdit

instance PathPiece Editable where
  fromPathPiece "normal"   = Just Normal
  fromPathPiece "editable" = Just Editable
  fromPathPiece "in-edit"  = Just InEdit
  fromPathPiece _          = Nothing
  toPathPiece Normal   = "normal"
  toPathPiece Editable = "editable"
  toPathPiece InEdit   = "in-edit"

class LiftJS a where liftJS :: a -> Text

instance LiftJS Text where liftJS = id
instance LiftJS Integer where liftJS = tshow
instance LiftJS Int where liftJS = tshow
instance LiftJS Editable where liftJS = tshow . toPathPiece

class JSParams a where
  jsParams :: a -> [Text]

instance JSParams () where
  jsParams () = []
instance LiftJS a => JSParams [a] where
  jsParams = map liftJS
instance (LiftJS a, LiftJS b) => JSParams (a,b) where
  jsParams (a,b) = [liftJS a, liftJS b]
instance (LiftJS a, LiftJS b, LiftJS c) => JSParams (a,b,c) where
  jsParams (a,b,c) = [liftJS a, liftJS b, liftJS c]
instance (LiftJS a, LiftJS b, LiftJS c, LiftJS d) => JSParams (a,b,c,d) where
  jsParams (a,b,c,d) = [liftJS a, liftJS b, liftJS c, liftJS d]

-- TODO: why not compare Haskellers too?
