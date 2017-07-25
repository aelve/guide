{-# LANGUAGE OverloadedStrings #-}

{- |
  Code Snippets renderer to Html
-}
module Snippets.Renderer
where

import           Imports

import qualified Data.IntMap     as IM
import qualified Data.Map        as M (lookup)

--import Text.Highlighting.Kate
-- Web
import           Lucid           hiding (for_)

import           Guide.Utils
import           Snippets.Parser

renderTestSnippets :: (MonadIO m) => IO (HtmlT m ())
renderTestSnippets = do
  nodes <- mainParse
  pure $ head_ $ do
    includeCSS "/snippets.css"
    title_ "Snippets – Aelve Guide"
    meta_ [ name_ "viewport"
          , content_ "width=device-width, initial-scale=1.0, user-scalable=yes"
          ]
    body_ $ renderSnippet nodes


renderSnippet :: (MonadIO m) => [[SnippetNode]] -> HtmlT m ()
renderSnippet [] = div_ "Empty Snippet"
renderSnippet x = do
  let (snpt, rest) = createLabels x
  if not (null snpt) then do
     createTabButtons snpt
     for_ snpt $ \lbl -> renderTab snpt lbl rest
     includeJS "/snippetTabs.js"
  else renderTab snpt (1, "singleSnippet") rest

createLabels :: [[SnippetNode]] -> ([(Int, Text)], [[SnippetNode]])
createLabels ([Multiple lbls]:xs) = (IM.assocs lbls, xs)
createLabels ([]:xs)              = createLabels xs
createLabels x                    = ([], x)

createTabButtons :: (MonadIO m) => [(Int, Text)] -> HtmlT m ()
createTabButtons [] = div_ "error"
createTabButtons lbls =
  div_ [class_ "tab"] $
    for_ lbls $ \l -> renderButton l
 where
  renderButton :: (MonadIO m) => (Int, Text) -> HtmlT m ()
  renderButton (1, lbl) =
    button_ [class_ "tablinks", onclick_ ("openCode(event , \"" <> lbl <> "\")"), id_ "defaultOpen"] $ toHtml lbl
  renderButton (_, lbl) =
    button_ [class_ "tablinks", onclick_ ("openCode(event , \"" <> lbl <> "\")")] $ toHtml lbl

renderTab :: (MonadIO m) => [(Int, Text)] -> (Int, Text) -> [[SnippetNode]] -> HtmlT m ()
renderTab lbls intLbl@(_, lbl) x =
  let clss = if not (null lbls) then " tabcontent" else "" in
  div_ [class_ ("code" <> clss), id_ lbl] $
    pre_ $
      for_ x (renderLine lbls intLbl)

renderLine :: (MonadIO m) => [(Int, Text)] -> (Int, Text) -> [SnippetNode] -> HtmlT m ()
renderLine lbls intLbl x =
  unless (null x) $
    case head x of
      HltLine -> mark_ [class_ "lineMark"] $ renderNode lbls intLbl (tail x)
      _       -> renderNode lbls intLbl x


renderNode :: (MonadIO m) => [(Int, Text)] -> (Int, Text) -> [SnippetNode] -> HtmlT m ()
renderNode _ _ [] = "\n"
renderNode lbls intLbl (HltBegin:xs) = do
  mark_ [class_ "inlineMark"] $ renderNode lbls intLbl (takeWhile (HltEnd /=) xs)
  renderNode lbls intLbl (tail $ dropWhile (HltEnd /=) xs)
renderNode lbls intLbl (CodeText t : xs) = do
  toHtml t
  renderNode lbls intLbl xs
renderNode lbls intLbl@(curInt, curLbl) (Choice x:xs) = do
  case M.lookup curLbl x of
    Just curTxt ->
      if curInt == 1
        then toHtml curTxt
      else
        case findLblByInd (pred curInt) lbls of
             Just prevLbl -> case M.lookup prevLbl x of
                                  Just prevTxt ->
                                    if prevTxt /= curTxt
                                      then span_ [class_ "diff"] $ toHtml curTxt
                                    else toHtml curTxt
                                  Nothing -> span_ [class_ "diff"] $ toHtml curTxt
             Nothing -> toHtml curTxt
    Nothing -> ""
  renderNode lbls intLbl xs
renderNode lbls lbl (Hackage x:xs) = do
  a_ [href_ "#"] (toHtml x)
  renderNode lbls lbl xs
renderNode lbls lbl (_:xs) = renderNode lbls lbl xs

findLblByInd :: Int -> [(Int, Text)] -> Maybe Text
findLblByInd _ [] = Nothing
findLblByInd n ((i, x):xs)
  | i == n = Just x
  | otherwise = findLblByInd n xs
