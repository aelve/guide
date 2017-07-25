{-# LANGUAGE OverloadedStrings #-}

{- |
  Code Snippets renderer to Html
-}
module Snippets.Renderer
where

import Imports

import qualified Data.Map as M (lookup)

--import Text.Highlighting.Kate
-- Web
import Lucid hiding (for_)

import Guide.Utils
import Snippets.Parser

renderTestSnippets :: (MonadIO m) => IO (HtmlT m ())
renderTestSnippets = do
  nodes <- mainParse
  pure $ head_ $ do
    includeJS "/snippetTabs.js"
    includeCSS "/snippets.css"
    title_ "Snippets – Aelve Guide"
    meta_ [name_ "viewport",
         content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]

    body_ $ renderSnippet nodes

renderSnippet :: (MonadIO m) => [[SnippetNode]] -> HtmlT m ()
renderSnippet [] = div_ "Empty Snippet"
renderSnippet x = do
  let (snpt, rest) = createLabels x
  unless (null snpt) $ createTabButtons snpt
  for_ snpt $ \lbl -> renderTab lbl rest

createLabels :: [[SnippetNode]] -> ([Text], [[SnippetNode]])
createLabels ([Multiple lbls]:xs) = (lbls, xs)
createLabels ([]:xs) = createLabels xs
createLabels x = ([], x)

createTabButtons :: (MonadIO m) => [Text] -> HtmlT m ()
createTabButtons [] = div_ "error"
createTabButtons (lbl:lbls) =
  div_ [class_ "tab"] $ do
    button_ [class_ "tablinks", onclick_ ("openCode(event , \"" <> lbl <> "\")"), id_ "defaultOpen"] $ toHtml lbl
    for_ lbls $ \l ->
      button_ [class_ "tablinks", onclick_ ("openCode(event , \"" <> l <> "\")")] $ toHtml l

renderTab :: (MonadIO m) => Text -> [[SnippetNode]] -> HtmlT m ()
renderTab lbl x = div_ [class_ "code tabcontent", id_ lbl] $
  pre_ $
    for_ x (renderLine lbl)

renderLine :: (MonadIO m) => Text -> [SnippetNode] -> HtmlT m ()
renderLine lbl x =
  unless (null x) $
    case head x of
      HltLine -> mark_ [class_ "lineMark"] $ renderNode lbl (tail x)
      _       -> renderNode lbl x


renderNode :: (MonadIO m) => Text -> [SnippetNode] -> HtmlT m ()
renderNode _ [] = "\n"
renderNode lbl (HltBegin:xs) = do
  mark_ [class_ "inlineMark"] $ renderNode lbl (takeWhile (HltEnd /=) xs)
  renderNode lbl(tail $ dropWhile (HltEnd /=) xs)
renderNode lbl (CodeText t : xs) = do
  toHtml t
  renderNode lbl xs
renderNode lbl (Choice x:xs) = do
  case M.lookup lbl x of
    Just txt -> toHtml txt
    Nothing -> ""
  renderNode lbl xs
renderNode lbl (Hackage x:xs) = do
  a_ [href_ "#"] (toHtml x)
  renderNode lbl xs
renderNode lbl (_:xs) = renderNode lbl xs