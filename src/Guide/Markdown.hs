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

  -- * Misc
  renderMD,
  markdownNull,
  extractPreface,
)
where


import           Imports

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
import           Lucid                 hiding (for_)
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
-- Safe
import           Safe                  (headDef, initDef, lastDef, tailDef)
-- Interpolation
import qualified NeatInterpolation     as NI

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

-- Orphan instances (to be deleted after migration to newer cmark-sections)
deriving instance (Data a) => Data (Annotated a)
deriving instance (Data a, Data b) => Data (Section a b)
deriving instance (Data a, Data b) => Data (Document a b)

makeFields ''MarkdownInline
makeFields ''MarkdownBlock
makeFields ''MarkdownTree

parseMD :: Text -> [MD.Node]
parseMD s =
  let MD.Document_ ns =
        highlightNode . shortcutLinks . commonmarkToNode [optSafe] $ s
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
      markdownBlockMdText     = annSource x,
      markdownBlockMdHtml     = renderMD (annValue x),
      markdownBlockMdMarkdown = annValue x }

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

shortcutLinks :: MD.Node -> MD.Node
shortcutLinks node@(MD.Link pos url title ns) | '@' <- T.head url =
  -- %20s are possibly introduced by cmark (Pandoc definitely adds them,
  -- no idea about cmark but better safe than sorry) and so they need to
  -- be converted back to spaces
  case parseLink (T.replace "%20" " " url) of
    Left _err -> MD.Link pos url title (map shortcutLinks ns)
    Right (shortcut, opt, text) -> do
      let text' = fromMaybe (stringify [node]) text
      let shortcuts = (["hk"], hackage) : allShortcuts
      case useShortcutFrom shortcuts shortcut opt text' of
        Success link ->
          MD.Link pos link title (map shortcutLinks ns)
        Warning warnings link ->
          let warningText = "[warnings when processing shortcut link: " <>
                            T.pack (intercalate ", " warnings) <> "]"
              warningNode = MD.Text_ warningText
          in  MD.Link pos link title (warningNode : map shortcutLinks ns)
        Failure err ->
          let errorText = "[error when processing shortcut link: " <>
                          T.pack err <> "]"
          in  MD.Text_ errorText
shortcutLinks (MD.Node pos tp ns) =
  MD.Node pos tp (map shortcutLinks ns)

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
             nodesToDocument (Ann s blocks)
    --
    toc :: Forest ([MD.Node], Text)  -- (heading, slug)
    toc = sections tree
            & each.each %~ (\Section{..} -> (annValue heading, headingAnn))

renderContents :: Document a b -> Document a ByteString
renderContents doc = doc {
  prefaceAnn = renderMD (annValue (preface doc)),
  sections = over (each.each) renderSection (sections doc) }
  where
    renderSection sec = sec {
      contentAnn = renderMD (annValue (content sec)) }

slugifyDocument :: (Text -> Text) -> Document a b -> Document Text b
slugifyDocument slugify doc = doc {
  sections = evalState ((each.each) process (sections doc)) mempty }
  where
    process :: Section a b -> State (Set Text) (Section Text b)
    process sec = do
      previousIds <- get
      let slug = until (`S.notMember` previousIds) (<> "_")
                   (slugify (stringify (annValue (heading sec))))
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
          toHtmlRaw (renderMD (annValue heading))
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

------------------------------
------ Markdown Tables -------
------------------------------

-- | Data Structure to hold tables
data Table = Table
           { name    :: Text -- ^ Table header
           , columns :: Maybe [[MD.Node]] -- ^ Names of columns (optional)
           , rows    :: [[[MD.Node]]] -- ^ List of rows with cells
           } deriving (Eq, Show)

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
    { name = "TableName"
    , columns = ["Column 1", "Column 2", "Column 3"]
    , rows = [ ["Foo", "Bar", "Baz"]
             , ["Another foo", "Another bar", "Another baz"]
             ]
    }
@
-}
getTable :: MD.Node -> Maybe Table
getTable (MD.ItemList_ _ (table:cols:brk:rest)) = do
  name <- getTableName table
  if isBreak brk then do
     let columns = getRow cols
     rows <- mapM getRow rest
     pure Table{..}
  else do
     guard (isBreak cols)
     let columns = Nothing
     rows <- mapM getRow (brk:rest)
     pure Table{..}

getTable _ = Nothing

-- | Parses table name after keyword "%TABLE"
getTableName :: [MD.Node] -> Maybe Text
getTableName [MD.Paragraph_ [MD.Text_ t]] = T.stripPrefix "%TABLE " t
getTableName _ = Nothing

-- | Gets whole row values
getRow :: [MD.Node] -> Maybe [[MD.Node]]
getRow [MD.ItemList_ _ items] = concat <$> mapM getCells items
getRow _                      = Nothing

{-|
Possible row syntax is
@
+  - smth 1
   - smth 2
   - smth 3
@
or
@
+  smth 1 | smth 2 | smth 3
@
These two examples are equal.
-}
getCells :: [MD.Node] -> Maybe [[MD.Node]]
getCells []     = Nothing
getCells items = Just $ splitCells [] items

splitCells :: [[MD.Node]] -> [MD.Node] -> [[MD.Node]]
splitCells = foldl' splitCell

-- Need to check if there were ' | ' in text blocks
splitCell :: [[MD.Node]] -> MD.Node -> [[MD.Node]]
splitCell res (MD.Paragraph_ x) = splitCells res x
splitCell res (MD.Text_ t) =
  let splited = (:[]) . MD.Text_ <$> T.splitOn "|" t in
  initDef [] res ++ ((lastDef [] res ++ headDef [] splited) : tailDef [] splited)
splitCell res x = initDef [] res ++ [lastDef [] res ++ [x]]

-- | Break line should separate table header (keyword & (optional) column names) from rows
isBreak :: [MD.Node] -> Bool
isBreak [MD.ThematicBreak_] = True
isBreak _                   = False

-- | Generates 'HTML' table from 'Table' structure
renderTable :: (Monad m) => Table -> HtmlT m ()
renderTable Table{..} = do
  h3_ $ toHtml name
  table_ [class_ "sortable"] $ do
    whenJust columns $ \clmns ->
      thead_ $ tr_ $
        for_ clmns $ \clmn ->
          td_ $ toHtml $ renderMD clmn
    tbody_ $
      for_ rows $ \row ->
        tr_ $ for_ row $ \cell ->
          td_ $ toHtml $ renderMD cell

-- testing Tables with example
{-
----------------
---- Table -----
----------------
    { name = "TableName"
    , columns =
        [ [ Node Nothing (TEXT "Column 1") [] ]
        , [ Node Nothing (TEXT "Column 2") [] ]
        , [ Node Nothing (TEXT "Column 3") [] ]
        ]
    , rows =
        [ [ [ Node Nothing EMPH [ Node Nothing (TEXT "foo") [] ]
            , Node Nothing (TEXT " ") []
            ]
          , [ Node Nothing (TEXT " ") []
            , Node Nothing STRONG [ Node Nothing (TEXT "bar") [] ]
            , Node Nothing (TEXT " ") []
            ]
          , [ Node Nothing (TEXT " baz") [] ]
          ]
        , [ [ Node Nothing (TEXT "Foo") [] ]
          , [ Node Nothing (TEXT "Bar") [] ]
          , [ Node Nothing (TEXT "Baz") [] ]
          ]
        , [ [ Node
                (Just
                   PosInfo
                     { startLine = 11
                     , startColumn = 5
                     , endLine = 13
                     , endColumn = 7
                     })
                (HTML_BLOCK
                   "<div class=\"sourceCode\"><pre class=\"sourceCode\"><code class=\"sourceCode\">Code foo</code></pre></div>")
                []
            ]
          , [ Node Nothing (TEXT "Simple bar") [] ]
          , [ Node Nothing (CODE "inline code baz") [] ]
          ]
        , [ [ Node Nothing (TEXT "Another foo ") [] ]
          , [ Node Nothing (TEXT " Another bar ") [] ]
          , [ Node Nothing (TEXT " Another baz") [] ]
          ]
        ]
    }

---------------
----- HTML ----
---------------
<h3>TableName</h3>
<table class="sortable">
  <thead>
    <tr>
      <td>Column 1</td>
      <td>Column 2</td>
      <td>Column 3</td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><em>foo</em></td>
      <td><strong>bar</strong></td>
      <td> baz</td>
    </tr>
    <tr>
      <td>Foo</td>
      <td>Bar</td>
      <td>Baz</td>
    </tr>
    <tr>
      <td>
        <div class="sourceCode">
          <pre class="sourceCode">
            <code class="sourceCode">Code foo</code>
          </pre>
        </div>
      </td>
      <td>Simple bar</td>
      <td><code>inline code baz</code></td>
    </tr>
    <tr>
      <td>Another foo </td>
      <td> Another bar </td>
      <td> Another baz</td>
    </tr>
  </tbody>
</table>
-}
testTable :: (Monad m) => HtmlT m ()
testTable = renderTable
          $ fromJust
          $ getTable
          $ head
          $ parseMD
          [NI.text|
              + %TABLE TableName
              + - Column 1
                - Column 2
                - Column 3
              + --------------------------------
              + - *foo* | **bar** | baz

              + - Foo
                - Bar
                - Baz

              + - ```
                  Code foo
                  ```
                - Simple bar
                - `inline code` baz
              + - Another foo | Another bar | Another baz
          |]