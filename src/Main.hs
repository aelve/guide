{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
RecordWildCards,
RankNTypes,
FlexibleInstances,
QuasiQuotes,
ScopedTypeVariables,
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
-- Randomness
import System.Random
-- Web
import Lucid hiding (for_)
import Web.Spock hiding (get, text)
import qualified Web.Spock as Spock
import Network.Wai.Middleware.Static


-- | Unique id, used for many things – categories, items, and anchor ids.
-- Note that in HTML 5 using numeric ids for divs, spans, etc is okay.
type UID = Int

randomUID :: MonadIO m => m UID
randomUID = liftIO $ randomRIO (0, 10^(9::Int))

data ProCon = ProCon {
  _proConId :: UID,
  _proConText :: Text }

makeLenses ''ProCon

data ItemKind = HackageLibrary | Library | Unknown

data Item = Item {
  _itemId :: UID,
  _name   :: Text,
  _pros   :: [ProCon],
  _cons   :: [ProCon],
  _link   :: Maybe Text,
  _kind   :: ItemKind }

makeLenses ''Item

proConById :: UID -> Lens' Item ProCon
proConById uid = singular $
  (pros.each . filtered ((== uid) . view proConId)) `failing`
  (cons.each . filtered ((== uid) . view proConId))

data Category = Category {
  _catId :: UID,
  _title :: Text,
  _items :: [Item] }

makeLenses ''Category

data S = S {
  _categories :: [Category] }

makeLenses ''S

categoryById :: UID -> Lens' S Category
categoryById uid = singular $
  categories.each . filtered ((== uid) . view catId)

itemById :: UID -> Lens' S Item
itemById uid = singular $
  categories.each . items.each . filtered ((== uid) . view itemId)

emptyState :: S
emptyState = S {
  _categories = [] }

sampleState :: S
sampleState = S {
  _categories = [
    Category {
      _catId = 1,
      _title = "lenses",
      _items = [
        Item {
          _itemId = 2,
          _name   = "lens",
          _pros   = [ProCon 3 "the standard lenses library",
                     ProCon 4 "batteries included"],
          _cons   = [ProCon 5 "huge"],
          _link   = Nothing,
          _kind   = HackageLibrary },
        Item {
          _itemId = 6,
          _name   = "microlens",
          _pros   = [ProCon 7 "very small",
                     ProCon 8 "good for libraries"],
          _cons   = [ProCon 9 "doesn't have advanced features"],
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
    uid <- randomUID
    let newCategory = Category {
          _catId = uid,
          _title = title',
          _items = [] }
    withS $
      categories %= (++ [newCategory])
    lucid $ renderCategory newCategory

  -- Create a new library in the specified category, with the library name
  -- and category id submitted via a POST request.
  Spock.post ("/category" <//> var <//> "library/add") $ \catId' -> do
    name' <- param' "name"
    uid <- randomUID
    let newItem = Item {
          _itemId = uid,
          _name   = name',
          _pros   = [],
          _cons   = [],
          _link   = Nothing,
          _kind   = HackageLibrary }
    -- TODO: maybe do something if the category doesn't exist (e.g. has been
    -- already deleted)
    withS $
      categoryById catId' . items %= (++ [newItem])
    lucid $ renderItem Normal newItem

  -- Add a pro (argument in favor of a library).
  Spock.post ("/item" <//> var <//> "pros/add") $ \itemId' -> do
    content <- param' "content"
    uid <- randomUID
    changedItem <- withS $ do
      itemById itemId' . pros %= (++ [ProCon uid content])
      use (itemById itemId')
    lucid $ renderItem Editable changedItem

  -- Add a con (argument against a library).
  Spock.post ("/item" <//> var <//> "cons/add") $ \itemId' -> do
    content <- param' "content"
    uid <- randomUID
    changedItem <- withS $ do
      itemById itemId' . cons %= (++ [ProCon uid content])
      use (itemById itemId')
    lucid $ renderItem Editable changedItem

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

  -- Return rendered item the way it should normally look.
  Spock.get ("/item" <//> var <//> "render-normal") $ \itemId' -> do
    item <- withS $ use (itemById itemId')
    lucid $ renderItem Normal item

  -- Return rendered item the way it should look when it's editable.
  Spock.get ("/item" <//> var <//> "render-edit") $ \itemId' -> do
    item <- withS $ use (itemById itemId')
    lucid $ renderItem Editable item

  -- Return rendered pro/con the way it should normally look.
  Spock.get ("/item" <//> var <//> "pro-con" <//> var <//> "render-normal") $
    \itemId' proConId' -> do
       thing <- withS $ use (itemById itemId' . proConById proConId')
       lucid $ renderProCon Editable itemId' thing

  -- Return rendered pro/con the way it should look when it's being edited.
  Spock.get ("/item" <//> var <//> "pro-con" <//> var <//> "render-edit") $
    \itemId' proConId' -> do
       thing <- withS $ use (itemById itemId' . proConById proConId')
       lucid $ renderProCon InEdit itemId' thing

  -- Change a pro/con.
  Spock.post ("/item" <//> var <//> "pro-con" <//> var <//> "set") $
    \itemId' proConId' -> do
       content <- param' "content"
       changedThing <- withS $ do
         itemById itemId' . proConById proConId' . proConText .= content
         use (itemById itemId' . proConById proConId')
       lucid $ renderProCon Editable itemId' changedThing

renderRoot :: S -> HtmlT IO ()
renderRoot s = do
  includeJS "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js"
  includeCSS "/css.css"
  -- Include definitions of all Javascript functions that we have defined in
  -- this file.
  script_ $ T.unlines (map snd (allJSFunctions :: [(Text, Text)]))
  categoriesNode <- div_ [id_ "categories"] $ do
    mapM_ renderCategory (s ^. categories)
    thisNode
  input_ [type_ "text", placeholder_ "new category",
          submitFunc (js_addCategory (categoriesNode, js_this_value))]

renderCategoryHeading :: Category -> HtmlT IO ()
renderCategoryHeading category =
  h2_ $ do
    headerNode <- thisNode
    -- TODO: make category headings anchor links
    toHtml (category^.title)
    textButton "edit" $
      js_startCategoryHeadingEdit (headerNode, category^.catId)

renderCategoryHeadingEdit :: Category -> HtmlT IO ()
renderCategoryHeadingEdit category =
  h2_ $ do
    headerNode <- thisNode
    let handler = js_submitCategoryHeadingEdit
                    (headerNode, category^.catId, js_this_value)
    input_ [type_ "text", value_ (category^.title), submitFunc handler]
    textButton "cancel" $
      js_cancelCategoryHeadingEdit (headerNode, category^.catId)

renderCategory :: Category -> HtmlT IO ()
renderCategory category =
  div_ [id_ (tshow (category^.catId))] $ do
    renderCategoryHeading category
    itemsNode <- div_ [class_ "items"] $ do
      mapM_ (renderItem Normal) (category^.items)
      thisNode
    let handler = js_addLibrary (itemsNode, category^.catId, js_this_value)
    input_ [type_ "text", placeholder_ "new item", submitFunc handler]

-- TODO: when the link for a HackageLibrary isn't empty, show it separately
-- (as “site”), don't replace the Hackage link
renderItem
  :: Editable         -- ^ Show edit buttons?
  -> Item
  -> HtmlT IO ()
renderItem editable item =
  div_ [class_ "item", id_ (tshow (item^.itemId))] $ do
    itemNode <- thisNode
    h3_ $ do
      itemHeader
      case editable of
        Normal -> textButton "edit" $
          js_enableItemEdit (itemNode, item^.itemId)
        Editable -> textButton "edit off" $
          js_disableItemEdit (itemNode, item^.itemId)
    div_ [class_ "pros-cons"] $ do
      div_ [class_ "pros"] $ do
        p_ "Pros:"
        case editable of
          Normal ->
            ul_ $ mapM_ (renderProCon Normal (item^.itemId)) (item^.pros)
          Editable -> do
            ul_ $ mapM_ (renderProCon Editable (item^.itemId)) (item^.pros)
            let handler = js_addPros (itemNode, item^.itemId, js_this_value)
            input_ [type_ "text", placeholder_ "add pros", submitFunc handler]
      div_ [class_ "cons"] $ do
        p_ "Cons:"
        case editable of
          Normal ->
            ul_ $ mapM_ (renderProCon Normal (item^.itemId)) (item^.cons)
          Editable -> do
            ul_ $ mapM_ (renderProCon Editable (item^.itemId)) (item^.cons)
            let handler = js_addCons (itemNode, item^.itemId, js_this_value)
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

renderProCon :: Editable -> UID -> ProCon -> HtmlT IO ()
renderProCon Normal _ thing = li_ (toHtml (thing^.proConText))
renderProCon Editable itemId' thing = li_ $ do
  this <- thisNode
  toHtml (thing^.proConText)
  textButton "edit" $
    js_startProConEdit (this, itemId', thing^.proConId)
renderProCon InEdit itemId' thing = li_ $ do
  this <- thisNode
  let handler = js_submitProConEdit
                  (this, itemId', thing^.proConId, js_this_value)
  input_ [type_ "text", value_ (thing^.proConText), submitFunc handler]
  textButton "cancel" $
    js_cancelProConEdit (this, itemId', thing^.proConId)

-- Utils

includeJS :: Monad m => Text -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Monad m => Text -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

submitFunc :: JS -> Attribute
submitFunc f = onkeyup_ $ format
  "if (event.keyCode == 13) {\
  \  {}\
  \  this.value = ''; }"
  [f]

-- Javascript

js_this_value :: JS
js_this_value = "this.value"

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
instance Format.Params a => JSFunction (a -> JS) where
  makeJSFunction fName _ = \args -> do
    let argsText = map (TL.toStrict . TL.toLazyText) (Format.buildParams args)
    fName <> "(" <> T.intercalate "," argsText <> ");"

allJSFunctions :: JSFunction a => [a]
allJSFunctions = [
  js_addLibrary, js_addCategory,
  js_startCategoryHeadingEdit, js_submitCategoryHeadingEdit, js_cancelCategoryHeadingEdit,
  js_addPros, js_addCons,
  js_enableItemEdit, js_disableItemEdit,
  js_startProConEdit, js_submitProConEdit, js_cancelProConEdit ]

-- | Create a new category.
js_addCategory :: JSFunction a => a
js_addCategory = makeJSFunction "addCategory" [text|
  function addCategory(node, s) {
    $.post("/category/add", {title: s})
     .done(function(data) {
       $(node).append(data);
       });
    }
  |]

-- | Add a new library to some category.
js_addLibrary :: JSFunction a => a
js_addLibrary = makeJSFunction "addLibrary" [text|
  function addLibrary(node, catId, s) {
    $.post("/category/"+catId+"/library/add", {name: s})
     .done(function(data) {
       $(node).append(data);
       });
    }
  |]

{- |
Start category heading editing (this happens when you click on “[edit]”).

This turns the heading into an editbox, and adds a [cancel] link.
-}
js_startCategoryHeadingEdit :: JSFunction a => a
js_startCategoryHeadingEdit = makeJSFunction "startCategoryHeadingEdit" [text|
  function startCategoryHeadingEdit(node, catId) {
    $.get("/category/"+catId+"/title/render-edit")
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

{- |
Cancel category heading editing.

This turns the heading with the editbox back into a simple text heading.
-}
js_cancelCategoryHeadingEdit :: JSFunction a => a
js_cancelCategoryHeadingEdit = makeJSFunction "cancelCategoryHeadingEdit" [text|
  function cancelCategoryHeadingEdit(node, catId) {
    $.get("/category/"+catId+"/title/render-normal")
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

{- |
Finish category heading editing (this happens when you submit the field).

This turns the heading with the editbox back into a simple text heading.
-}
js_submitCategoryHeadingEdit :: JSFunction a => a
js_submitCategoryHeadingEdit = makeJSFunction "submitCategoryHeadingEdit" [text|
  function submitCategoryHeadingEdit(node, catId, s) {
    $.post("/category/"+catId+"/title/set", {title: s})
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

-- | Add pros to some item.
js_addPros :: JSFunction a => a
js_addPros = makeJSFunction "addPros" [text|
  function addPros(node, itemId, s) {
    $.post("/item/"+itemId+"/pros/add", {content: s})
     .done(function(data) {
       // update the whole item container
       $(node).replaceWith(data);
       });
    }
  |]

-- | Add cons to some item.
js_addCons :: JSFunction a => a
js_addCons = makeJSFunction "addCons" [text|
  function addCons(node, itemId, s) {
    $.post("/item/"+itemId+"/cons/add", {content: s})
     .done(function(data) {
       // update the whole item container
       $(node).replaceWith(data);
       });
    }
  |]

-- | Add “[edit]” buttons to everything in an item.
js_enableItemEdit :: JSFunction a => a
js_enableItemEdit = makeJSFunction "enableItemEdit" [text|
  function enableItemEdit (node, itemId) {
    $.get("/item/"+itemId+"/render-edit")
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

-- | Remove “[edit]” buttons from everything in an item.
js_disableItemEdit :: JSFunction a => a
js_disableItemEdit = makeJSFunction "disableItemEdit" [text|
  function disableItemEdit (node, itemId) {
    $.get("/item/"+itemId+"/render-normal")
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

js_startProConEdit :: JSFunction a => a
js_startProConEdit = makeJSFunction "startProConEdit" [text|
  function startProConEdit(node, itemId, thingId) {
    $.get("/item/"+itemId+"/pro-con/"+thingId+"/render-edit")
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

js_cancelProConEdit :: JSFunction a => a
js_cancelProConEdit = makeJSFunction "cancelProConEdit" [text|
  function cancelProConEdit(node, itemId, thingId) {
    $.get("/item/"+itemId+"/pro-con/"+thingId+"/render-normal")
     .done(function(data) {
       $(node).replaceWith(data);
       });
    }
  |]

js_submitProConEdit :: JSFunction a => a
js_submitProConEdit = makeJSFunction "submitProConEdit" [text|
  function submitProConEdit(node, itemId, thingId, s) {
    $.post("/item/"+itemId+"/pro-con/"+thingId+"/set", {content: s})
     .done(function(data) {
       $(node).replaceWith(data);
       });
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
  uid <- randomUID
  span_ [id_ (tshow uid)] mempty
  return (T.pack (show (format ":has(> #{})" [uid])))

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
