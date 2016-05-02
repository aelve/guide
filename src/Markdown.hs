{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
MultiParamTypeClasses,
FunctionalDependencies,
FlexibleInstances,
FlexibleContexts,
NoImplicitPrelude
  #-}


module Markdown
(
  -- * Types
  MarkdownInline(..),
  MarkdownBlock(..),
  MarkdownBlockWithTOC(..),

  -- * Lenses
  mdHtml,
  mdText,
  mdMarkdown,
  mdIdPrefix,
  mdTOC,

  -- * Rendering
  renderMarkdownInline,
  renderMarkdownBlock,
  renderMarkdownBlockWithTOC,

  -- * Misc
  markdownNull,
)
where


-- General
import BasePrelude hiding (Space)
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monad transformers and monads
import Control.Monad.Writer
import Control.Monad.State
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- ByteString
import qualified Data.ByteString.Lazy as BSL
-- Parsing
import Text.Megaparsec hiding (State)
-- HTML
import Lucid
-- Containers
import Data.Sequence ((<|), singleton)
import Data.Tree
import qualified Data.Set as S
import Data.Set (Set)
-- Markdown
import Cheapskate
import Cheapskate.Lucid
import Cheapskate.Highlight
import ShortcutLinks
import ShortcutLinks.All (hackage)
-- acid-state
import Data.SafeCopy

-- Local
import Utils


data MarkdownInline = MarkdownInline {
  markdownInlineMdText     :: Text,
  markdownInlineMdHtml     :: BSL.ByteString,
  markdownInlineMdMarkdown :: !Inlines }

data MarkdownBlock = MarkdownBlock {
  markdownBlockMdText     :: Text,
  markdownBlockMdHtml     :: BSL.ByteString,
  markdownBlockMdMarkdown :: !Blocks }

data MarkdownBlockWithTOC = MarkdownBlockWithTOC {
  markdownBlockWithTOCMdText     :: Text,
  markdownBlockWithTOCMdHtml     :: BSL.ByteString,
  markdownBlockWithTOCMdMarkdown :: !Blocks,
  markdownBlockWithTOCMdIdPrefix :: Text,
  markdownBlockWithTOCMdTOC      :: Forest (Inlines, Text) }

makeFields ''MarkdownInline
makeFields ''MarkdownBlock
makeFields ''MarkdownBlockWithTOC

genTOC
  :: (Text -> Text)                    -- ^ Function for generating a slug
  -> Blocks                            -- ^ Markdown
  -> (Forest (Inlines, Text), Blocks)  -- ^ TOC and modified blocks
genTOC slugify blocks =
  let (blocks', (_, headers)) = runState (mapM process blocks) (mempty, [])
  in  (makeTOC (reverse headers), blocks')
  where
    makeTOC :: [(Int, Inlines, Text)] -> Forest (Inlines, Text)
    makeTOC [] = []
    makeTOC ((level,contents,slug):xs) =
      let (sub, rest) = span ((>level) . view _1) xs
      in  Node (contents, slug) (makeTOC sub) : makeTOC rest
    --
    process :: Block -> State (Set Text, [(Int, Inlines, Text)]) Block
    process (Header n is) = do
      previousIds <- use _1
      let slug = until (`S.notMember` previousIds) (<> "_")
                   (slugify (stringify is))
      _1 %= S.insert slug
      _2 %= ((n, is, slug):)
      let anchor = RawHtml ("<span id='" <> slug <> "'></span>")
      return (Header n (anchor <| is))
    process b = return b

-- | Convert a Markdown structure to a string with formatting removed.
stringify :: Inlines -> Text
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
      let text' = fromMaybe (stringify (singleton i)) text
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
renderMarkdownInline s = MarkdownInline s (renderBS md) inlines
  where
    Doc opts blocks = markdown def s
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
renderMarkdownBlock s = MarkdownBlock {
  markdownBlockMdText     = s,
  markdownBlockMdHtml     = renderBS md,
  markdownBlockMdMarkdown = blocks }
  where
    Doc opts blocks = highlightDoc . walk shortcutLinks . markdown def $ s
    md = renderDoc (Doc opts blocks)

renderMarkdownBlockWithTOC :: Text -> Text -> MarkdownBlockWithTOC
renderMarkdownBlockWithTOC idPrefix s = MarkdownBlockWithTOC {
  markdownBlockWithTOCMdText     = s,
  markdownBlockWithTOCMdHtml     = renderBS md,
  markdownBlockWithTOCMdMarkdown = blocks',
  markdownBlockWithTOCMdIdPrefix = idPrefix,
  markdownBlockWithTOCMdTOC      = toc }
  where
    Doc opts blocks = highlightDoc . walk shortcutLinks . markdown def $ s
    (toc, blocks') = let slugify x = idPrefix <> makeSlug x
                     in  genTOC slugify blocks
    md = renderDoc (Doc opts blocks')

instance Show MarkdownInline where
  show = show . view mdText
instance Show MarkdownBlock where
  show = show . view mdText
instance Show MarkdownBlockWithTOC where
  show = show . view mdText

instance ToHtml MarkdownInline where
  toHtml    = toHtmlRaw . view mdHtml
  toHtmlRaw = toHtmlRaw . view mdHtml
instance ToHtml MarkdownBlock where
  toHtml    = toHtmlRaw . view mdHtml
  toHtmlRaw = toHtmlRaw . view mdHtml
instance ToHtml MarkdownBlockWithTOC where
  toHtml    = toHtmlRaw . view mdHtml
  toHtmlRaw = toHtmlRaw . view mdHtml

instance SafeCopy MarkdownInline where
  version = 0
  kind = base
  putCopy = contain . safePut . view mdText
  getCopy = contain $ renderMarkdownInline <$> safeGet
instance SafeCopy MarkdownBlock where
  version = 0
  kind = base
  putCopy = contain . safePut . view mdText
  getCopy = contain $ renderMarkdownBlock <$> safeGet
instance SafeCopy MarkdownBlockWithTOC where
  version = 0
  kind = base
  putCopy md = contain $ do
    safePut (md ^. mdIdPrefix)
    safePut (md ^. mdText)
  getCopy = contain $
    renderMarkdownBlockWithTOC <$> safeGet <*> safeGet

markdownNull :: HasMdText a Text => a -> Bool
markdownNull = T.null . view mdText
