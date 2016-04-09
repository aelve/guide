{-# LANGUAGE
OverloadedStrings,
NoImplicitPrelude
  #-}


module Markdown
(
  renderMarkdownInline,
  renderMarkdownBlock,
  MarkdownInline(..),
  MarkdownBlock(..),
)
where


-- General
import BasePrelude hiding (Space)
-- Monad transformers and monads
import Control.Monad.Writer
import Data.Functor.Identity
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Parsing
import Text.Megaparsec
-- HTML
import Lucid
import Lucid.Base
import Blaze.ByteString.Builder (Builder)
-- Sequence (used by Cheapskate)
import Data.Sequence
-- Markdown
import Cheapskate
import Cheapskate.Lucid
import Cheapskate.Highlight
import ShortcutLinks
import ShortcutLinks.All (hackage)
-- acid-state
import Data.SafeCopy


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
      let shortcuts = (["hk"], hackage) : allShortcuts
      case useShortcutFrom shortcuts shortcut opt text' of
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

renderMarkdownInline :: Text -> MarkdownInline
renderMarkdownInline s = MarkdownInline s (htmlToBuilder md)
  where
    Doc opts blocks = markdown def{allowRawHtml=False} s
    inlines = extractInlines =<< blocks
    md = renderInlines opts (walk shortcutLinks inlines)
    --
    extractInlines (Para xs) = xs
    extractInlines (Header _ xs) = xs
    extractInlines (Blockquote bs) = extractInlines =<< bs
    extractInlines (List _ _ bss) = extractInlines =<< mconcat bss
    extractInlines (CodeBlock _ x) = pure (Code x)
    extractInlines (HtmlBlock x) = pure (Code x)
    extractInlines HRule = mempty

renderMarkdownBlock :: Text -> MarkdownBlock
renderMarkdownBlock s = MarkdownBlock s (htmlToBuilder md)
  where
    md = renderDoc . highlightDoc . walk shortcutLinks . markdown def $ s

data MarkdownInline = MarkdownInline {
  markdownInlineText :: Text,
  markdownInlineHtml :: !Builder }

data MarkdownBlock = MarkdownBlock {
  markdownBlockText :: Text,
  markdownBlockHtml :: !Builder }

instance Eq MarkdownInline where
  (==) = (==) `on` markdownInlineText
instance Eq MarkdownBlock where
  (==) = (==) `on` markdownBlockText

instance Show MarkdownInline where
  show = show . markdownInlineText

instance Show MarkdownBlock where
  show = show . markdownBlockText

instance ToHtml MarkdownInline where
  toHtml    = builderToHtml . markdownInlineHtml
  toHtmlRaw = builderToHtml . markdownInlineHtml
instance ToHtml MarkdownBlock where
  toHtml    = builderToHtml . markdownBlockHtml
  toHtmlRaw = builderToHtml . markdownBlockHtml

builderToHtml :: Monad m => Builder -> HtmlT m ()
builderToHtml b = HtmlT (return (\_ -> b, ()))

htmlToBuilder :: Html () -> Builder
htmlToBuilder = runIdentity . execHtmlT

instance IsString MarkdownInline where
  fromString = renderMarkdownInline . fromString
instance IsString MarkdownBlock where
  fromString = renderMarkdownBlock . fromString

instance SafeCopy MarkdownInline where
  version = 0
  kind = base
  putCopy = contain . safePut . markdownInlineText
  getCopy = contain $ renderMarkdownInline <$> safeGet
instance SafeCopy MarkdownBlock where
  version = 0
  kind = base
  putCopy = contain . safePut . markdownBlockText
  getCopy = contain $ renderMarkdownBlock <$> safeGet
