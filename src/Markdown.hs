{-# LANGUAGE
OverloadedStrings,
NoImplicitPrelude
  #-}


module Markdown
(
  renderMarkdownLine,
  renderMarkdownBlock,
)
where


-- General
import BasePrelude hiding (Space)
-- Monad transformers and monads
import Control.Monad.Writer
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Parsing
import Text.Megaparsec
-- HTML
import Lucid
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Blaze.Html as Blaze
-- Seq (used by Cheapskate)
import Data.Sequence
-- Markdown
import Cheapskate
import Cheapskate.Html
import ShortcutLinks


blazeToLucid :: Monad m => Blaze.Html -> HtmlT m ()
blazeToLucid = toHtmlRaw . Blaze.renderHtml

-- | Convert a Markdown structure to a string with formatting removed.
stringify :: Inline -> Text
stringify = execWriter . walkM go
  where
    go :: Inline -> Writer Text Inline
    go i = do
      case i of
        Str x -> tell x
        Space -> tell " "
        SoftBreak -> tell " "
        LineBreak -> tell " "
        Code x -> tell x
        -- TODO: this should be @<convert-entity-to-text> x@
        Entity _ -> return ()
        RawHtml _ -> return ()
        _ -> return ()
      return i

shortcutLinks :: Inline -> Inline
shortcutLinks i@(Link is url title) | '@' <- T.head url =
  -- %20s are possibly introduced by Cheapskate (Pandoc definitely adds them,
  -- no idea about Cheapskate but better safe than sorry) and so they need to
  -- be converted back to spaces
  case parseLink (T.replace "%20" " " url) of
    Left _err -> i
    Right (shortcut, opt, text) -> do
      let text' = fromMaybe (stringify i) text
      case useShortcut shortcut opt text' of
        Success link ->
          Link is link title
        Warning warnings link ->
          let w = Str $ "[warnings when processing shortcut link: " <>
                        T.pack (intercalate ", " warnings) <> "]"
          in  Link (w <| is) link title
        Failure err ->
          Str ("[error when processing shortcut link: " <> T.pack err <> "]")
shortcutLinks other = other

-- TODO: this should be in the shortcut-links package itself

-- | Parse a shortcut link. Allowed formats:
--
-- @
-- \@name
-- \@name:text
-- \@name(option)
-- \@name(option):text
-- @
parseLink :: Text -> Either String (Text, Maybe Text, Maybe Text)
parseLink = either (Left . show) Right . parse p ""
  where
    shortcut = some (alphaNumChar <|> char '-')
    opt      = char '(' *> some (noneOf ")") <* char ')'
    text     = char ':' *> some anyChar
    p = do
      char '@'
      (,,) <$> T.pack <$> shortcut
           <*> optional (T.pack <$> opt)
           <*> optional (T.pack <$> text)

-- TODO: rename to renderMarkdownInline
renderMarkdownLine :: Monad m => Text -> HtmlT m ()
renderMarkdownLine s = do
  let Doc opts blocks = markdown def{allowRawHtml=False} s
      inlines = extractInlines =<< blocks
  blazeToLucid (renderInlines opts (walk shortcutLinks inlines))
  where
    extractInlines (Para xs) = xs
    extractInlines (Header _ xs) = xs
    extractInlines (Blockquote bs) = extractInlines =<< bs
    extractInlines (List _ _ bss) = extractInlines =<< mconcat bss
    extractInlines (CodeBlock _ x) = pure (Code x)
    extractInlines (HtmlBlock x) = pure (Code x)
    extractInlines HRule = mempty

-- TODO: rename to renderMarkdownBlocks
-- TODO: would be nice to have syntax highlighting
renderMarkdownBlock :: Monad m => Text -> HtmlT m ()
renderMarkdownBlock =
  blazeToLucid . renderDoc .
  walk shortcutLinks . markdown def{allowRawHtml=False}
