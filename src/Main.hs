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


data Item
  = Library {
      _name :: Text }
  | Something {
      _name :: Text }

makeLenses ''Item

data Category = Category {
  _categoryId :: Int,
  _categoryTitle :: Text,
  _categoryItems :: [Item] }

makeLenses ''Category

data S = S {
  _nextId :: Int,
  _categories :: [Category] }

makeLenses ''S

categoryById :: Int -> Traversal' S Category
categoryById catId = categories . each . filtered ((== catId) . _categoryId)

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  middleware (staticPolicy (addBase "static"))
  stateVar <- liftIO $ newIORef S {
    _nextId = 0,
    _categories = [] }
  get root $ do
    s <- liftIO $ readIORef stateVar
    lucid $ renderRoot s
  post "/add/category" $ do
    title <- param' "title"
    thisId <- liftIO $ view nextId <$> readIORef stateVar
    let newCategory = Category {
          _categoryId = thisId,
          _categoryTitle = title,
          _categoryItems = [] }
    liftIO $ modifyIORef stateVar $
      (categories %~ (++ [newCategory])) .
      (nextId %~ succ)
    lucid $ renderCategory newCategory
  post ("/add/item/library" <//> var) $ \catId -> do
    libName <- param' "name"
    let newItem = Library {
          _name = libName }
    -- TODO: maybe do something if the category doesn't exist (e.g. has been
    -- already deleted)
    liftIO $ modifyIORef stateVar $
      categoryById catId . categoryItems %~ (++ [newItem])
    lucid $ renderItem newItem

renderRoot :: S -> Html ()
renderRoot s = do
  loadJS "https://ajax.googleapis.com/ajax/libs/jquery/2.2.0/jquery.min.js"
  loadJS "/js.js"
  div_ [id_ "categories"] $ do
    mapM_ renderCategory (s ^. categories)
  let handler = "if (event.keyCode == 13) {\
                \  addCategory(this.value);\
                \  this.value = ''; }"
  input_ [type_ "text", placeholder_ "new category", onkeyup_ handler]

renderCategory :: Category -> Html ()
renderCategory Category{..} =
  div_ [id_ (format "cat{}" [_categoryId])] $ do
    -- TODO: make category headings links
    h2_ (toHtml _categoryTitle)
    ul_ $ mapM_ (li_ . renderItem) _categoryItems
    -- TODO: probably move handlers to the Javascript part
    let handler = format "if (event.keyCode == 13) {\
                         \  addLibrary({}, this.value);\
                         \  this.value = ''; }"
                         [_categoryId]
    input_ [type_ "text", placeholder_ "new item", onkeyup_ handler]

renderItem :: Item -> Html ()
renderItem Library{..} = a_ [href_ link] (toHtml _name)
  where
    link = format "https://hackage.haskell.org/package/{}" [_name]
renderItem Something{..} = toHtml _name

-- Utils

loadJS :: Text -> Html ()
loadJS url = with (script_ "") [src_ url]

lucid :: Html a -> ActionT IO a
lucid = html . TL.toStrict . renderText

-- | Format a string (a bit 'Text.Printf.printf' but with different syntax).
format :: Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)
