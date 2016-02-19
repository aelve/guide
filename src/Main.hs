{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
RecordWildCards,
RankNTypes,
FlexibleInstances,
QuasiQuotes,
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
import qualified Data.Text.Lazy.Builder as TL
import Data.Text.Format hiding (format)
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
import NeatInterpolation
-- Web
import Lucid hiding (for_)
import Web.Spock hiding (get, text)
import qualified Web.Spock as Spock
import Network.Wai.Middleware.Static


-- | Unique id, used for categories and items.
type UID = Int

data ItemKind = HackageLibrary | Library | Unknown

data Item = Item {
  _itemId :: UID,
  _name   :: Text,
  _pros   :: [Text],
  _cons   :: [Text],
  _link   :: Maybe Text,
  _kind   :: ItemKind }

makeLenses ''Item

data Category = Category {
  _catId :: UID,
  _title :: Text,
  _items :: [Item] }

makeLenses ''Category

data S = S {
  _nextId :: UID,
  _categories :: [Category] }

makeLenses ''S

categoryById :: UID -> Lens' S Category
categoryById uid = singular $
  categories.each . filtered ((== uid) . view catId)

itemById :: UID -> Lens' S Item
itemById uid = singular $
  categories.each . items.each . filtered ((== uid) . view itemId)

newId :: IORef S -> IO UID
newId s = do
  uid <- view nextId <$> readIORef s
  modifyIORef s (nextId %~ succ)
  return uid

emptyState :: S
emptyState = S {
  _nextId = 0,
  _categories = [] }

sampleState :: S
sampleState = S {
  _nextId = 3,
  _categories = [
    Category {
      _catId = 0,
      _title = "lenses",
      _items = [
        Item {
          _itemId = 1,
          _name   = "lens",
          _pros   = ["the standard lenses library", "batteries included"],
          _cons   = ["huge"],
          _link   = Nothing,
          _kind   = HackageLibrary },
        Item {
          _itemId = 2,
          _name   = "microlens",
          _pros   = ["very small", "good for libraries"],
          _cons   = ["doesn't have advanced features"],
          _link   = Nothing,
          _kind   = HackageLibrary }
      ] }
  ] }

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  middleware (staticPolicy (addBase "static"))
  stateVar <- liftIO $ newIORef sampleState
  let withS :: MonadIO m => State S a -> m a
      withS f = liftIO $ atomicModifyIORef' stateVar (swap . runState f)

  -- Render the main page.
  Spock.get root $ do
    s <- liftIO $ readIORef stateVar
    lucid $ renderRoot s

  -- The “/add” methods return rendered parts of the structure (added
  -- categories, changed items, etc) so that the Javascript part could take
  -- them and inject into the page. We don't want to duplicate rendering on
  -- server side and on client side.

  -- (category|item)/action
  -- (category|item)/id/action
  -- (category|item)/id/thing/action

  -- Create a new category, with its title submitted via a POST request.
  Spock.post "/category/add" $ do
    title' <- param' "title"
    id' <- liftIO (newId stateVar)
    let newCategory = Category {
          _catId = id',
          _title = title',
          _items = [] }
    withS $
      categories %= (++ [newCategory])
    lucid $ renderCategory newCategory

  -- Create a new library in the specified category, with the library name
  -- and category id submitted via a POST request.
  Spock.post ("/category" <//> var <//> "library/add") $ \catId' -> do
    name' <- param' "name"
    id' <- liftIO (newId stateVar)
    let newItem = Item {
          _itemId = id',
          _name   = name',
          _pros   = [],
          _cons   = [],
          _link   = Nothing,
          _kind   = HackageLibrary }
    -- TODO: maybe do something if the category doesn't exist (e.g. has been
    -- already deleted)
    withS $
      categoryById catId' . items %= (++ [newItem])
    lucid $ renderItem newItem

  -- Add a pro (argument in favor of a library).
  Spock.post ("/item" <//> var <//> "pros/add") $ \itemId' -> do
    content <- param' "content"    
    changedItem <- withS $ do
      itemById itemId' . pros %= (++ [content])
      use (itemById itemId')
    lucid $ renderItem changedItem

  -- Add a con (argument against a library).
  Spock.post ("/item" <//> var <//> "cons/add") $ \itemId' -> do
    content <- param' "content"    
    changedItem <- withS $ do
      itemById itemId' . cons %= (++ [content])
      use (itemById itemId')
    lucid $ renderItem changedItem

  -- Set the title of a category (returns rendered new title).
  Spock.post ("/category" <//> var <//> "title/set") $ \catId' -> do
    title' <- param' "title"
    changedCategory <- withS $ do
      categoryById catId' . title .= title'
      use (categoryById catId')
    lucid $ renderCategoryHeading changedCategory

  -- Return rendered title of a category.
  Spock.get ("/category" <//> var <//> "title/render-normal") $ \catId' -> do
    category <- withS $ use (categoryById catId')
    lucid $ renderCategoryHeading category

  -- Return rendered title of a category the way it should look when the
  -- category is being edited.
  Spock.get ("/category" <//> var <//> "title/render-edit") $ \catId' -> do
    category <- withS $ use (categoryById catId')
    lucid $ renderCategoryHeadingEdit category

renderRoot :: S -> Html ()
renderRoot s = do
  includeJS "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js"
  includeCSS "/css.css"
  -- Include definitions of all Javascript functions that we have defined in
  -- this file.
  script_ $ T.unlines (map snd (allJSFunctions :: [(Text, Text)]))
  div_ [id_ "categories"] $ do
    mapM_ renderCategory (s ^. categories)
  input_ [type_ "text", placeholder_ "new category",
          submitFunc (js_addCategory [js_this_value])]

renderCategoryHeading :: Category -> Html ()
renderCategoryHeading category =
  h2_ $ do
    -- TODO: make category headings anchor links
    toHtml (category^.title)
    textButton "edit" $
      js_startCategoryHeadingEdit [category^.catId]

renderCategoryHeadingEdit :: Category -> Html ()
renderCategoryHeadingEdit category =
  h2_ $ do
    let handler = js_submitCategoryHeadingEdit
                    (category^.catId, js_this_value)
    input_ [type_ "text", value_ (category^.title), submitFunc handler]
    textButton "cancel" $ js_cancelCategoryHeadingEdit [category^.catId]

renderCategory :: Category -> Html ()
renderCategory category =
  div_ [id_ (format "cat{}" [category^.catId])] $ do
    renderCategoryHeading category
    -- Note: if you change anything here, look at js.js/addLibrary to see
    -- whether it has to be updated.
    div_ [class_ "items"] $
      mapM_ renderItem (category^.items)
    let handler = js_addLibrary (category^.catId, js_this_value)
    input_ [type_ "text", placeholder_ "new item", submitFunc handler]

-- TODO: when the link for a HackageLibrary isn't empty, show it separately
-- (as “site”), don't replace the Hackage link
renderItem :: Item -> Html ()
renderItem item =
  div_ [class_ "item", id_ (format "item{}" [item^.itemId])] $ do
    h3_ itemHeader
    div_ [class_ "pros-cons"] $ do
      div_ [class_ "pros"] $ do
        p_ "Pros:"
        ul_ $ mapM_ (li_ . toHtml) (item^.pros)
        let handler = js_addPros (item^.itemId, js_this_value)
        input_ [type_ "text", placeholder_ "add pros", submitFunc handler]
      div_ [class_ "cons"] $ do
        p_ "Cons:"
        ul_ $ mapM_ (li_ . toHtml) (item^.cons)
        let handler = js_addCons (item^.itemId, js_this_value)
        input_ [type_ "text", placeholder_ "add cons", submitFunc handler]
  where
    hackageLink = format "https://hackage.haskell.org/package/{}"
                         [item^.name]
    itemHeader = case (item^.link, item^.kind) of
      (Just l, _) ->
        a_ [href_ l] (toHtml (item^.name))
      (Nothing, HackageLibrary) ->
        a_ [href_ hackageLink] (toHtml (item^.name))
      _otherwise -> toHtml (item^.name)

-- Utils

includeJS :: Text -> Html ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Text -> Html ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

submitFunc :: Text -> Attribute
submitFunc f = onkeyup_ $ format
  "if (event.keyCode == 13) {\
  \  {}\
  \  this.value = ''; }"
  [f]

-- Javascript

js_this_value :: Text
js_this_value = "this.value"

class JSFunction a where
  makeJSFunction
    :: Text          -- Name
    -> Text          -- Definition
    -> a

-- This generates function name
instance JSFunction Text where
  makeJSFunction fName _ = fName

-- This generates function definition and direct dependencies
instance JSFunction (Text, Text) where
  makeJSFunction fName fDef = (fName, fDef)

-- This generates a function that takes arguments and produces a Javascript
-- function call
instance Format.Params a => JSFunction (a -> Text) where
  makeJSFunction fName _ = \args -> do
    let argsText = map (TL.toStrict . TL.toLazyText) (Format.buildParams args)
    fName <> "(" <> T.intercalate "," argsText <> ");"

allJSFunctions :: JSFunction a => [a]
allJSFunctions = [
  js_addLibrary, js_addCategory,
  js_startCategoryHeadingEdit, js_submitCategoryHeadingEdit, js_cancelCategoryHeadingEdit,
  js_addPros, js_addCons,
  js_setItemHtml, js_setCategoryHeadingHtml ]

-- | Create a new category.
js_addCategory :: JSFunction a => a
js_addCategory = makeJSFunction "addCategory" [text|
  function addCategory(s) {
    $.post("/category/add", {title: s})
     .done(function(data) {
       $("#categories").append(data);
       });
    }
  |]

-- | Add a new library to some category.
js_addLibrary :: JSFunction a => a
js_addLibrary = makeJSFunction "addLibrary" [text|
  function addLibrary(catId, s) {
    $.post("/category/"+catId+"/library/add", {name: s})
     .done(function(data) {
       $("#cat"+catId+" > .items").append(data);
       });
    }
  |]

{- |
Start category heading editing (this happens when you click on “[edit]”).

This turns the heading into an editbox, and adds a [cancel] link.
-}
js_startCategoryHeadingEdit :: JSFunction a => a
js_startCategoryHeadingEdit = makeJSFunction "startCategoryHeadingEdit" [text|
  function startCategoryHeadingEdit(catId) {
    $.get("/category/"+catId+"/title/render-edit")
     .done(function(data) {
       setCategoryHeadingHtml(catId, data);
       });
    }
  |]

{- |
Cancel category heading editing.

This turns the heading with the editbox back into a simple text heading.
-}
js_cancelCategoryHeadingEdit :: JSFunction a => a
js_cancelCategoryHeadingEdit = makeJSFunction "cancelCategoryHeadingEdit" [text|
  function cancelCategoryHeadingEdit(catId) {
    $.get("/category/"+catId+"/title/render-normal")
     .done(function(data) {
       setCategoryHeadingHtml(catId, data);
       });
    }
  |]

{- |
Finish category heading editing (this happens when you submit the field).

This turns the heading with the editbox back into a simple text heading.
-}
js_submitCategoryHeadingEdit :: JSFunction a => a
js_submitCategoryHeadingEdit = makeJSFunction "submitCategoryHeadingEdit" [text|
  function submitCategoryHeadingEdit(catId, s) {
    $.post("/category/"+catId+"/title/set", {title: s})
     .done(function(data) {
       setCategoryHeadingHtml(catId, data);
       });
    }
  |]

-- | Add pros to some item.
js_addPros :: JSFunction a => a
js_addPros = makeJSFunction "addPros" [text|
  function addPros(itemId, s) {
    $.post("/item/"+itemId+"/pros/add", {content: s})
     .done(function(data) {
       setItemHtml(itemId, data);
       });
    }
  |]

-- | Add cons to some item.
js_addCons :: JSFunction a => a
js_addCons = makeJSFunction "addCons" [text|
  function addCons(itemId, s) {
    $.post("/item/"+itemId+"/cons/add", {content: s})
     .done(function(data) {
       setItemHtml(itemId, data);
       });
    }
  |]

-- | Reload an item.
js_setItemHtml :: JSFunction a => a
js_setItemHtml = makeJSFunction "setItemHtml" [text|
  function setItemHtml(itemId, data) {
    $("#item"+itemId).replaceWith(data);
    }
  |]

-- | Reload a category heading.
js_setCategoryHeadingHtml :: JSFunction a => a
js_setCategoryHeadingHtml = makeJSFunction "setCategoryHeadingHtml" [text|
  function setCategoryHeadingHtml(catId, data) {
    $("#cat"+catId+" > h2").replaceWith(data);
    }
  |]

-- A text button looks like “[cancel]”
textButton
  :: Text    -- ^ Button text
  -> Text    -- ^ Onclick handler
  -> Html ()
textButton caption handler =
  span_ [class_ "textButton"] $
    a_ [href_ "javascript:void(0)", onclick_ handler] (toHtml caption)

lucid :: Html a -> ActionT IO a
lucid = html . TL.toStrict . renderText

-- | Format a string (a bit 'Text.Printf.printf' but with different syntax).
format :: Format.Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)
