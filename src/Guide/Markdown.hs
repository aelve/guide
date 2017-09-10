{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes        #-}


{- |
Everything concerning rendering and processing Markdown.

Currently we use the @cmark@ package as the Markdown parser.
-}
module Guide.Markdown
(
  -- * Types
  MarkdownInline(..),
  MarkdownBlock(..),
  MarkdownTree(..),

  -- * Lenses
  mdHtml,
  mdText,
  mdMarkdown,
  mdIdPrefix,
  mdTree,
  mdTOC,

  -- * Converting text to Markdown
  toMarkdownInline,
  toMarkdownBlock,
  toMarkdownTree,
  parseMD,
  parseVanillaMD,

  -- * Misc
  renderMD,
  markdownNull,
  extractPreface,

  -- * Tables
  MarkdownTable(..),
  getTable,
  renderTable,
)
where


import           Imports

-- Lists
import           Data.List.Split       (splitOn)
-- Text
import qualified Data.Text.All         as T
-- ByteString
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
-- Parsing
import           Text.Megaparsec       hiding (State)
import           Text.Megaparsec.Text
-- JSON
import qualified Data.Aeson            as A
-- HTML
import           Lucid                 as Lucid hiding (for_)
import           Text.HTML.SanitizeXSS
-- Containers
import qualified Data.Set              as S
import           Data.Tree
-- Markdown
import           CMark                 hiding (Node)
import qualified CMark                 as MD
import qualified CMark.Patterns        as MD
import           CMark.Highlight
import           CMark.Sections
import           ShortcutLinks
import           ShortcutLinks.All     (hackage)
-- acid-state
import           Data.SafeCopy

import           Control.Monad.Extra   (whenJust)

import           Guide.Utils


data MarkdownInline = MarkdownInline {
  markdownInlineMdText     :: Text,
  markdownInlineMdHtml     :: ByteString,
  markdownInlineMdMarkdown :: ![MD.Node] }
  deriving (Generic, Data)

data MarkdownBlock = MarkdownBlock {
  markdownBlockMdText     :: Text,
  markdownBlockMdHtml     :: ByteString,
  markdownBlockMdMarkdown :: ![MD.Node] }
  deriving (Generic, Data)

data MarkdownTree = MarkdownTree {
  markdownTreeMdText     :: Text,
  markdownTreeMdTree     :: !(Document Text ByteString),
  markdownTreeMdIdPrefix :: Text,
  markdownTreeMdTOC      :: Forest ([MD.Node], Text) }
  deriving (Generic, Data)

makeFields ''MarkdownInline
makeFields ''MarkdownBlock
makeFields ''MarkdownTree

-- | Parse Markdown and apply our custom passes to it.
parseMD :: Text -> [MD.Node]
parseMD s =
  let MD.Document_ ns =
        renderTables .                -- render tables (as HTML)
        highlightNode .               -- highlight code everywhere
        resolveShortcutLinks $        -- turn shortcut links into ordinary ones
        commonmarkToNode [optSafe] s  -- parse Markdown
  in  ns

-- | Parse Markdown without doing anything fancy.
parseVanillaMD :: Text -> [MD.Node]
parseVanillaMD s =
  let MD.Document_ ns = commonmarkToNode [optSafe] s
  in  ns

renderMD :: [MD.Node] -> ByteString
renderMD ns
  -- See https://github.com/jgm/cmark/issues/147
  | any isInlineNode ns =
      T.toByteString . sanitize . T.concat . map (nodeToHtml []) $ ns
  | otherwise =
      T.toByteString . sanitize . nodeToHtml [] $ MD.Document_ ns

isInlineNode :: MD.Node -> Bool
isInlineNode (MD.Node _ tp _) = case tp of
  EMPH              -> True
  STRONG            -> True
  LINK _ _          -> True
  CUSTOM_INLINE _ _ -> True
  SOFTBREAK         -> True
  LINEBREAK         -> True
  TEXT _            -> True
  CODE _            -> True
  HTML_INLINE _     -> True
  _other            -> False

-- | Convert a Markdown structure to a string with formatting removed.
stringify :: [MD.Node] -> Text
stringify = T.concat . map go
  where
    go (MD.Node _ tp ns) = case tp of
      DOCUMENT          -> stringify ns
      THEMATIC_BREAK    -> stringify ns
      PARAGRAPH         -> stringify ns
      BLOCK_QUOTE       -> stringify ns
      CUSTOM_BLOCK _ _  -> stringify ns
      HEADING _         -> stringify ns
      LIST _            -> stringify ns
      ITEM              -> stringify ns
      EMPH              -> stringify ns
      STRONG            -> stringify ns
      LINK _ _          -> stringify ns
      IMAGE _ _         -> stringify ns
      CUSTOM_INLINE _ _ -> stringify ns
      CODE         xs   -> xs
      CODE_BLOCK _ xs   -> xs
      TEXT         xs   -> xs
      SOFTBREAK         -> " "
      LINEBREAK         -> " "
      HTML_BLOCK _      -> ""
      HTML_INLINE _     -> ""

-- | Extract everything before the first heading.
--
-- Note that if you render 'mdText' of the produced Markdown block, it won't
-- necessarily parse into 'mdHtml' from the same block. It's because rendered
-- Markdown might depend on links that are defined further in the tree.
extractPreface :: MarkdownTree -> MarkdownBlock
extractPreface = mkBlock . preface . view mdTree
  where
    mkBlock x = MarkdownBlock {
      markdownBlockMdText     = getSource x,
      markdownBlockMdHtml     = renderMD (stripSource x),
      markdownBlockMdMarkdown = stripSource x }

-- | Flatten Markdown by concatenating all block elements.
extractInlines :: [MD.Node] -> [MD.Node]
extractInlines = concatMap go
  where
    go node@(MD.Node _ tp ns) = case tp of
      -- Block containers
      DOCUMENT          -> extractInlines ns
      BLOCK_QUOTE       -> extractInlines ns
      CUSTOM_BLOCK _ _  -> extractInlines ns
      LIST _            -> extractInlines ns
      ITEM              -> extractInlines ns
      -- Inline containers
      PARAGRAPH         -> ns
      HEADING _         -> ns
      IMAGE _ _         -> ns
      -- Inlines
      EMPH              -> [node]
      STRONG            -> [node]
      LINK _ _          -> [node]
      CUSTOM_INLINE _ _ -> [node]
      SOFTBREAK         -> [node]
      LINEBREAK         -> [node]
      TEXT _            -> [node]
      CODE _            -> [node]
      -- Other stuff
      THEMATIC_BREAK    -> []
      HTML_BLOCK xs     -> [MD.Code_ xs]
      HTML_INLINE xs    -> [MD.Code_ xs]
      CODE_BLOCK _ xs   -> [MD.Code_ xs]

-- | Resolve all shortcut links in a Markdown node. If a link is invalid, it
-- will insert an error message instead.
resolveShortcutLinks :: MD.Node -> MD.Node
resolveShortcutLinks node@(MD.Link pos url title ns) | '@' <- T.head url =
  -- %20s are possibly introduced by cmark (Pandoc definitely adds them,
  -- no idea about cmark but better safe than sorry) and so they need to
  -- be converted back to spaces
  case parseLink (T.replace "%20" " " url) of
    Left _err -> MD.Link pos url title (map resolveShortcutLinks ns)
    Right (shortcut, opt, text) -> do
      let text' = fromMaybe (stringify [node]) text
      let shortcuts = (["hk"], hackage) : allShortcuts
      case useShortcutFrom shortcuts shortcut opt text' of
        Success link ->
          MD.Link pos link title (map resolveShortcutLinks ns)
        Warning warnings link ->
          let warningText = "[warnings when processing a shortcut link: " <>
                            T.pack (intercalate ", " warnings) <> "]"
              warningNode = MD.Text_ warningText
          in  MD.Link pos link title
                  (warningNode : map resolveShortcutLinks ns)
        Failure err ->
          let errorText = "[error when processing a shortcut link: " <>
                          T.pack err <> "]"
          in  MD.Text_ errorText
resolveShortcutLinks (MD.Node pos tp ns) =
  MD.Node pos tp (map resolveShortcutLinks ns)

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
    opt      = char '(' *> some (noneOf [')']) <* char ')'
    text     = char ':' *> some anyChar
    p :: Parser (Text, Maybe Text, Maybe Text)
    p = do
      char '@'
      (,,) <$> T.pack <$> shortcut
           <*> optional (T.pack <$> opt)
           <*> optional (T.pack <$> text)

toMarkdownInline :: Text -> MarkdownInline
toMarkdownInline s = MarkdownInline {
  markdownInlineMdText     = s,
  markdownInlineMdHtml     = html,
  markdownInlineMdMarkdown = inlines }
  where
    inlines = extractInlines (parseMD s)
    html = renderMD inlines

toMarkdownBlock :: Text -> MarkdownBlock
toMarkdownBlock s = MarkdownBlock {
  markdownBlockMdText     = s,
  markdownBlockMdHtml     = html,
  markdownBlockMdMarkdown = doc }
  where
    doc = parseMD s
    html = renderMD doc

toMarkdownTree :: Text -> Text -> MarkdownTree
toMarkdownTree idPrefix s = MarkdownTree {
  markdownTreeMdText     = s,
  markdownTreeMdIdPrefix = idPrefix,
  markdownTreeMdTree     = tree,
  markdownTreeMdTOC      = toc }
  where
    blocks :: [MD.Node]
    blocks = parseMD s
    --
    slugify :: Text -> Text
    slugify x = idPrefix <> makeSlug x
    --
    tree :: Document Text ByteString
    tree = renderContents . slugifyDocument slugify $
             nodesToDocument (WithSource s blocks)
    --
    toc :: Forest ([MD.Node], Text)  -- (heading, slug)
    toc = sections tree
            & each.each %~ (\Section{..} -> (stripSource heading, headingAnn))

renderContents :: Document a b -> Document a ByteString
renderContents doc = doc {
  prefaceAnn = renderMD (stripSource (preface doc)),
  sections = over (each.each) renderSection (sections doc) }
  where
    renderSection sec = sec {
      contentAnn = renderMD (stripSource (content sec)) }

slugifyDocument :: (Text -> Text) -> Document a b -> Document Text b
slugifyDocument slugify doc = doc {
  sections = evalState ((each.each) process (sections doc)) mempty }
  where
    process :: Section a b -> State (Set Text) (Section Text b)
    process sec = do
      previousIds <- get
      let slug = until (`S.notMember` previousIds) (<> "_")
                   (slugify (stringify (stripSource (heading sec))))
      modify (S.insert slug)
      return sec{headingAnn = slug}

instance Show MarkdownInline where
  show = show . view mdText
instance Show MarkdownBlock where
  show = show . view mdText
instance Show MarkdownTree where
  show = show . view mdText

instance A.ToJSON MarkdownInline where
  toJSON md = A.object [
    "text" A..= (md^.mdText),
    "html" A..= T.toStrict (md^.mdHtml) ]
instance A.ToJSON MarkdownBlock where
  toJSON md = A.object [
    "text" A..= (md^.mdText),
    "html" A..= T.toStrict (md^.mdHtml) ]
instance A.ToJSON MarkdownTree where
  toJSON md = A.object [
    "text" A..= (md^.mdText) ]

instance ToHtml MarkdownInline where
  toHtmlRaw = toHtml
  toHtml    = toHtmlRaw . view mdHtml
instance ToHtml MarkdownBlock where
  toHtmlRaw = toHtml
  toHtml    = toHtmlRaw . view mdHtml
instance ToHtml MarkdownTree where
  toHtmlRaw = toHtml
  toHtml    = toHtmlRaw . renderDoc . view mdTree
    where
      renderDoc Document{..} = BS.concat $
        prefaceAnn :
        map renderSection (concatMap flatten sections)
      renderSection Section{..} = BSL.toStrict . renderBS $ do
        mkH $ do
          span_ [id_ headingAnn] ""
          toHtmlRaw (renderMD (stripSource heading))
        toHtmlRaw contentAnn
        where
          mkH = case level of
            1 -> h1_; 2 -> h2_; 3 -> h3_;
            4 -> h4_; 5 -> h5_; 6 -> h6_;
            _other -> error "Markdown.toHtml: level > 6"

instance SafeCopy MarkdownInline where
  version = 0
  kind = base
  putCopy = contain . safePut . view mdText
  getCopy = contain $ toMarkdownInline <$> safeGet
instance SafeCopy MarkdownBlock where
  version = 0
  kind = base
  putCopy = contain . safePut . view mdText
  getCopy = contain $ toMarkdownBlock <$> safeGet
instance SafeCopy MarkdownTree where
  version = 0
  kind = base
  putCopy md = contain $ do
    safePut (md ^. mdIdPrefix)
    safePut (md ^. mdText)
  getCopy = contain $
    toMarkdownTree <$> safeGet <*> safeGet

-- | Is a piece of Markdown empty?
markdownNull :: HasMdText a Text => a -> Bool
markdownNull = T.null . view mdText

----------------------------------------------------------------------------
-- Support for tables
----------------------------------------------------------------------------

-- | A table in Markdown is described by this type.
data MarkdownTable = MarkdownTable
  { markdownTableName    :: Maybe Text        -- ^ Table header
  , markdownTableColumns :: Maybe [[MD.Node]] -- ^ Names of columns
  , markdownTableRows    :: [[[MD.Node]]]     -- ^ List of rows with cells
  } deriving (Eq, Show, Data)

data TableParsingError
  = NotATable Text
  | WrongRowFormat [MD.Node]
  deriving (Eq, Show)

renderTableParsingError :: TableParsingError -> [MD.Node]
renderTableParsingError = \case
  NotATable reason ->
    [ MD.Paragraph_ [MD.Text_ ("not a table ("+|reason|+")")] ]
  WrongRowFormat nodes ->
    [ MD.Paragraph_ [MD.Text_ "wrong row format (expected a list \
                              \or a paragraph) for this row:"]
    , MD.BlockQuote_ nodes ]

{-|
Tries to make 'Table' structure from Node.
Next markdown
@
+ %TABLE TableName
+ - Column 1
  - Column 2
  - Column 3
+ --------------------------------

+ - Foo
  - Bar
  - Baz

+ - Another foo | Another bar | Another baz
@
should be parsed as
@
Table
    { name = Just "TableName"
    , columns = Just ["Column 1", "Column 2", "Column 3"]
    , rows = [ ["Foo", "Bar", "Baz"]
             , ["Another foo", "Another bar", "Another baz"]
             ]
    }
@
-}
getTable :: MD.Node -> Either TableParsingError MarkdownTable
getTable node = do
  (table, mbHeader, rows) <- case node of
      MD.ListItems_ _ (table:header:[MD.ThematicBreak_]:rows) ->
        pure (table, Just header, rows)
      MD.ListItems_ _ (table:[MD.ThematicBreak_]:rows) ->
        pure (table, Nothing, rows)
      MD.ListItems_ _ _ ->
        Left (NotATable "a table must have a ThematicBreak")
      MD.Node _ tp _ ->
        Left (NotATable ("a table must be a list, not a "
                         +|takeWhile (/= ' ') (show tp)|+""))
  markdownTableName    <- getTableName table
  markdownTableColumns <- mapM getCells mbHeader
  markdownTableRows    <- mapM getCells rows
  pure MarkdownTable{..}

-- | Parses table name after keyword "%TABLE"
getTableName :: [MD.Node] -> Either TableParsingError (Maybe Text)
getTableName [MD.Paragraph_ [MD.Text_ t]] = do
  name <- case T.strip <$> T.stripPrefix "%TABLE" t of
    Nothing -> Left (NotATable "a table must start with %TABLE")
    Just x  -> Right x
  pure $ if T.null name then Nothing else Just name
getTableName _ = Left $ NotATable
  "a table must start with %TABLE in a single paragraph without markup"

-- | Extract cells from a row description. A row can be specified either by
-- a list (each item containing one cell), or by a line containing cells
-- separated by "|".
getCells :: [MD.Node] -> Either TableParsingError [[MD.Node]]
getCells = \case
  [MD.ListItems_ _ xs] -> Right xs
  [MD.Paragraph_ s]    -> Right (splitRow s)
  other                -> Left (WrongRowFormat other)

-- | Split Markdown separated by pipe characters. In pseudocode:
--
-- >>> splitRow "foo | **bar baz** blah | `qux`"
-- ["foo ", " **bar baz** blah ", " `qux`"]
--
-- Note that @cmark@ always wraps text into paragraphs. For uniformity, when
-- we split the row manually, we do the same.
splitRow :: [MD.Node] -> [[MD.Node]]
splitRow = map (\s -> [MD.Paragraph_ s]) .
           splitOn [MD.Text_ "|"] .
           concatMap splitText
  where
    splitText (MD.Text_ s) = map MD.Text_ $ intersperse "|" $ T.splitOn "|" s
    splitText other        = [other]

-- | Generates 'HTML' table from 'Table' structure
renderTable :: (Monad m) => MarkdownTable -> HtmlT m ()
renderTable MarkdownTable{..} = do
  case markdownTableName of
    Just name -> h3_ $ toHtml name
    Nothing   -> pure ()
  table_ [class_ "sortable"] $ do
    whenJust markdownTableColumns $ \cols ->
      thead_ $ tr_ $
        for_ cols $ \col ->
          td_ $ toHtmlRaw $ renderMD col
    tbody_ $
      for_ markdownTableRows $ \row ->
        tr_ $ for_ row $ \cell ->
          td_ $ toHtmlRaw $ renderMD cell

-- | Find and render all tables in a Markdown node. If a table can't be
-- parsed, it will insert an error message instead (generated with
-- 'renderTableParsingError').
--
-- Note that other passes (e.g. 'resolveShortcutLinks') should happen before
-- this one, because the rendered tables are inserted as HTML and it's not
-- possible to inspect their contents anymore.
renderTables :: MD.Node -> MD.Node
renderTables (MD.Node pos tp ns) =
    case getTable node' of
      Left (NotATable _) ->
        node'
      Left err -> MD.HtmlBlock pos $
        T.toStrict $ renderMD $ renderTableParsingError err
      Right table -> MD.HtmlBlock pos $
        T.toStrict $ Lucid.renderText $ renderTable table
  where
    node' = MD.Node pos tp (map renderTables ns)
