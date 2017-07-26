{-# LANGUAGE OverloadedStrings #-}

{-|
  Code Snippets renderer to Html
-}
module Snippets.Renderer
where

import           Imports

import           Data.List                       (dropWhileEnd)
import qualified Data.Map                        as M (lookup)
import qualified Data.Text                       as T
-- Web
import           Lucid                           hiding (for_)

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Highlighting.Kate

import           Guide.Utils
import           Snippets.Parser

{-|
Renders given text example to Lucid
-}
renderTestSnippets :: (MonadIO m) => HtmlT m ()
renderTestSnippets = do
  nodes <- liftIO mainParse
  head_ $ do
    includeCSS "/snippets.css"
    includeCSS "/highlight.css"
    title_ "Snippets – Aelve Guide"
    meta_ [ name_ "viewport"
          , content_ "width=device-width, initial-scale=1.0, user-scalable=yes"
          ]
    body_ $ renderSnippet nodes

-- Doesn't create tab if no multiple snippets
-- In this case 'renderTab' works with fake "singleSnippet" label
renderSnippet :: (Monad m) => Snippet -> HtmlT m ()
renderSnippet [] = div_ "Empty Snippet"
renderSnippet x  = do
  let (snpt, rest) = createLabels x
  if not (null snpt) then do
     createTabButtons snpt
     for_ snpt $ \lbl -> renderTab snpt lbl rest
     includeJS "/snippetTabs.js"
  else renderTab snpt (1, "singleSnippet") rest

-- in case of 'Multiple' was in the first not empty line
-- creates list of labels (with order number)
-- returns empty one otherwise
createLabels :: Snippet -> ([(Int, Text)], Snippet)
createLabels ([Multiple lbls]:xs) = (lbls, xs)
createLabels ([]:xs)              = createLabels xs
createLabels x                    = ([], x)

-- in case of multiple snippets Tab-Panel should be created
createTabButtons :: (Monad m) => [(Int, Text)] -> HtmlT m ()
createTabButtons []   = div_ "error"
createTabButtons lbls =
  div_ [class_ "tab"] $
    for_ lbls renderButton
 where
  renderButton :: (Monad m) => (Int, Text) -> HtmlT m ()
  renderButton (1, lbl) =
    button_ [ class_ "tablinks"
            , onclick_ ("openCode(event , \"" <> lbl <> "\")")
            , id_ "defaultOpen"
            ] $ toHtml lbl
  renderButton (_, lbl) =
    button_ [ class_ "tablinks"
            , onclick_ ("openCode(event , \"" <> lbl <> "\")")
            ] $ toHtml lbl

{-|
Render snippet for specific label tag
-}
renderTab :: (Monad m)
          => [(Int, Text)] -- ^ All Labels with order numbers
          -> (Int, Text) -- ^ Current rendering label
          -> Snippet  -- ^ Rendering lines
          -> HtmlT m () -- ^ Rendered tab content
renderTab lbls intLbl@(_, lbl) x =
  let clss = if not (null lbls) then " tabcontent" else "" in
  div_ [class_ ("code sourceCode" <> clss), id_ lbl] $
    pre_ $
      for_ x (renderLine lbls intLbl)

{-|
Render line (list of snippet nodes) for specific label tag
-}
renderLine :: (Monad m)
           => [(Int, Text)] -- ^ All Labels with order numbers
           -> (Int, Text) -- ^ Current rendering label
           -> SnippetLine -- ^ Rendering line
           -> HtmlT m () -- ^ Rendered list of nodes
renderLine lbls intLbl (HltLine:rest) =
  mark_ [class_ "lineMark"] $ renderNode lbls intLbl rest
renderLine lbls intLbl nodes =
  renderNode lbls intLbl nodes

{-|
Render snippet node for specific label tag
-}
renderNode :: (Monad m)
           => [(Int, Text)] -- ^ All Labels with order numbers
           -> (Int, Text) -- ^ Current rendering label
           -> SnippetLine -- ^ Rendering line
           -> HtmlT m () -- ^ Rendered node
renderNode _ _ [] = "\n"
renderNode lbls intLbl (HltBegin:xs) = do
  let (before, after) = span (/= HltEnd) xs
  mark_ [class_ "inlineMark"] $ renderNode lbls intLbl before
  renderNode lbls intLbl (drop 1 after)
renderNode lbls intLbl (CodeText t:xs) = do
  renderTextHtml t
  renderNode lbls intLbl xs
renderNode lbls intLbl@(curInt, curLbl) (Choice x:xs) = do
  case M.lookup curLbl x of
    Just curTxt ->
      if curInt == 1
        then renderTextHtml curTxt
      else
        case lookup (pred curInt) lbls of
             Just prevLbl -> case M.lookup prevLbl x of
                                  Just prevTxt ->
                                    if prevTxt /= curTxt
                                      then span_ [class_ "diff"] $ renderTextHtml curTxt
                                    else renderTextHtml curTxt
                                  Nothing -> span_ [class_ "diff"] $ renderTextHtml curTxt
             Nothing -> renderTextHtml curTxt
    Nothing -> ""
  renderNode lbls intLbl xs
renderNode lbls lbl (Hackage x:xs) = do
  a_ [href_ "#"] (toHtml x)
  renderNode lbls lbl xs
renderNode lbls lbl (_:xs) = renderNode lbls lbl xs

highlightKateCode :: Text -> String
highlightKateCode code = renderHtml $ formatHtmlInline defaultFormatOpts
                         $ highlightAs "haskell" (T.unpack code)

renderTextHtml :: (Monad m) => Text -> HtmlT m ()
renderTextHtml = toHtmlRaw . highlightKateCode