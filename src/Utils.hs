{-# LANGUAGE
OverloadedStrings,
NoImplicitPrelude
  #-}


module Utils
(
  -- * Text
  format,
  tshow,

  -- * Lists
  moveUp,
  moveDown,
  deleteFirst,

  -- * URLs
  Url,
  sanitiseUrl,

  -- * Lucid
  includeJS,
  includeCSS,
  renderMarkdownLine,
  renderMarkdownBlock,

  -- * Spock
  lucid,
)
where


-- General
import BasePrelude
-- Monads and monad transformers
import Control.Monad.IO.Class
-- Text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
-- Formatting
import Data.Text.Format hiding (format)
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Format
-- Web
import Lucid
import Web.Spock
import Text.HTML.SanitizeXSS (sanitaryURI)
-- blaze-html (cheapskate uses it, so we need to be able to convert)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Blaze.Html as Blaze
-- Markdown
import Cheapskate
import Cheapskate.Html


-- | Format a string (a bit like 'Text.Printf.printf' but with different
-- syntax). The version in "Data.Text.Format" returns lazy text, but we
-- use strict text everywhere.
format :: Format.Params ps => Format -> ps -> Text
format f ps = TL.toStrict (Format.format f ps)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Move the -1st element that satisfies the predicate- up.
moveUp :: (a -> Bool) -> [a] -> [a]
moveUp p (x:y:xs) = if p y then (y:x:xs) else x : moveUp p (y:xs)
moveUp _ xs = xs

-- | Move the -1st element that satisfies the predicate- down.
moveDown :: (a -> Bool) -> [a] -> [a]
moveDown p (x:y:xs) = if p x then (y:x:xs) else x : moveDown p (y:xs)
moveDown _ xs = xs

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _   []   = []
deleteFirst f (x:xs) = if f x then xs else x : deleteFirst f xs

type Url = Text

sanitiseUrl :: Url -> Maybe Url
sanitiseUrl u
  | not (sanitaryURI u)       = Nothing
  | "http:" `T.isPrefixOf` u  = Just u
  | "https:" `T.isPrefixOf` u = Just u
  | otherwise                 = Just ("http://" <> u)

includeJS :: Monad m => Url -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Monad m => Url -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

-- TODO: rename to renderMarkdownInline
renderMarkdownLine :: Monad m => Text -> HtmlT m ()
renderMarkdownLine s = do
  let Doc opts blocks = markdown def{allowRawHtml=False} s
      inlines = extractInlines =<< blocks
  blazeToLucid (renderInlines opts inlines)
  where
    extractInlines (Para xs) = xs
    extractInlines (Header _ xs) = xs
    extractInlines (Blockquote bs) = extractInlines =<< bs
    extractInlines (List _ _ bss) = extractInlines =<< mconcat bss
    extractInlines (CodeBlock _ x) = pure (Code x)
    extractInlines (HtmlBlock x) = pure (Code x)
    extractInlines HRule = mempty

-- TODO: rename to renderMarkdownBlocks
-- TODO: use shortcut-links
-- TODO: would be nice to have syntax highlighting
renderMarkdownBlock :: Monad m => Text -> HtmlT m ()
renderMarkdownBlock =
  blazeToLucid . renderDoc . markdown def{allowRawHtml=False}

blazeToLucid :: Monad m => Blaze.Html -> HtmlT m ()
blazeToLucid = toHtmlRaw . Blaze.renderHtml

lucid :: MonadIO m => HtmlT IO a -> ActionCtxT ctx m a
lucid h = do
  htmlText <- liftIO (renderTextT h)
  html (TL.toStrict htmlText)
