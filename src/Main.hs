{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
RecordWildCards,
RankNTypes,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude hiding (Category)
-- Lenses
import Lens.Micro.Platform
-- IO
import Control.Monad.IO.Class
-- Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format hiding (format)
import qualified Data.Text.Format as Format
import Data.Text.Format.Params (Params)
-- Web
import Lucid hiding (for_)
import Web.Spock
import Network.Wai.Middleware.Static


data ItemKind = HackageLibrary | Library | Unknown

data Item = Item {
  _name :: Text,
  _pros :: [Text],
  _cons :: [Text],
  _link :: Maybe Text,
  _kind :: ItemKind }

makeLenses ''Item

data Category = Category {
  _catId :: Int,
  _title :: Text,
  _items :: [Item] }

makeLenses ''Category

data S = S {
  _nextId :: Int,
  _categories :: [Category] }

makeLenses ''S

categoryById :: Int -> Traversal' S Category
categoryById i = categories . each . filtered ((== i) . view catId)

newId :: IORef S -> IO Int
newId s = do
  i <- view nextId <$> readIORef s
  modifyIORef s (nextId %~ succ)
  return i

emptyState :: S
emptyState = S {
  _nextId = 0,
  _categories = [] }

sampleState :: S
sampleState = S {
  _nextId = 1,
  _categories = [
    Category {
      _catId = 0,
      _title = "lenses",
      _items = [
        Item {
          _name = "lens",
          _pros = ["the standard lenses library", "batteries included"],
          _cons = ["huge"],
          _link = Nothing,
          _kind = HackageLibrary },
        Item {
          _name = "microlens",
          _pros = ["very small", "good for libraries"],
          _cons = ["doesn't have advanced features"],
          _link = Nothing,
          _kind = HackageLibrary }
      ] }
  ] }

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  middleware (staticPolicy (addBase "static"))
  stateVar <- liftIO $ newIORef sampleState

  get root $ do
    s <- liftIO $ readIORef stateVar
    lucid $ renderRoot s

  post "/add/category" $ do
    title' <- param' "title"
    id' <- liftIO (newId stateVar)
    let newCategory = Category {
          _catId = id',
          _title = title',
          _items = [] }
    liftIO $ modifyIORef stateVar $
      categories %~ (++ [newCategory])
    lucid $ renderCategory newCategory

  post ("/add/item/library" <//> var) $ \catId' -> do
    name' <- param' "name"
    let newItem = Item {
          _name = name',
          _pros = [],
          _cons = [],
          _link = Nothing,
          _kind = HackageLibrary }
    -- TODO: maybe do something if the category doesn't exist (e.g. has been
    -- already deleted)
    liftIO $ modifyIORef stateVar $
      categoryById catId' . items %~ (++ [newItem])
    lucid $ renderItem newItem

renderRoot :: S -> Html ()
renderRoot s = do
  includeJS "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js"
  includeJS "/js.js"
  includeCSS "/css.css"
  div_ [id_ "categories"] $ do
    mapM_ renderCategory (s ^. categories)
  let handler = "if (event.keyCode == 13) {\
                \  addCategory(this.value);\
                \  this.value = ''; }"
  input_ [type_ "text", placeholder_ "new category", onkeyup_ handler]

renderCategory :: Category -> Html ()
renderCategory category =
  div_ [id_ (format "cat{}" [category^.catId])] $ do
    -- TODO: make category headings links
    h2_ (toHtml (category^.title))
    -- Note: if you change anything here, look at js.js/addLibrary to see
    -- whether it has to be updated.
    div_ [class_ "items"] $
      mapM_ renderItem (category^.items)
    -- TODO: probably move handlers to the Javascript part
    let handler = format "if (event.keyCode == 13) {\
                         \  addLibrary({}, this.value);\
                         \  this.value = ''; }"
                         [category^.catId]
    input_ [type_ "text", placeholder_ "new item", onkeyup_ handler]

-- TODO: when the link for a HackageLibrary isn't empty, show it separately
-- (as “site”), don't replace the Hackage link
renderItem :: Item -> Html ()
renderItem item =
  div_ [class_ "item"] $ do
    h3_ itemHeader
    div_ [class_ "pros-cons"] $ do
      div_ [class_ "pros"] $ do
        p_ "Pros:"
        ul_ $ mapM_ (li_ . toHtml) (item^.pros)
      div_ [class_ "cons"] $ do
        p_ "Cons:"
        ul_ $ mapM_ (li_ . toHtml) (item^.cons)
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

lucid :: Html a -> ActionT IO a
lucid = html . TL.toStrict . renderText

-- | Format a string (a bit 'Text.Printf.printf' but with different syntax).
format :: Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)
