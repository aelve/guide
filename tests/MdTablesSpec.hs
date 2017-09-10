{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module MdTablesSpec (tests) where

import           BasePrelude
import           Control.Monad.IO.Class      (liftIO)
-- CMark
import qualified CMark                       as MD
-- Text
import           Data.Text.All               (Text, LText)
import qualified Data.Text.All               as T
-- Testing
import           Test.Hspec
-- Generics
import           Data.Generics.Uniplate.Data (transformBi)
-- Lucid
import           Lucid.Base                  (renderTextT)

import           Guide.Markdown
import           Guide.Utils

tests :: Spec
tests =
  describe "MdTables.table" $ do

    describe "getting" $ do
      let shouldParseAs md res =
            hspecFailDetails (T.unlines md) $
            fmap stripPos (getTable (buildTableMD md)) `shouldBe`
              Right (stripPos res)
          shouldNotParse md =
            hspecFailDetails (T.unlines md) $
            getTable (buildTableMD md) `shouldSatisfy` isLeft
      it "parses a simple table with column names from list" $
        simpleTableColListMD `shouldParseAs` simpleTable
      it "parses a simple table with column names from text with separator" $
        simpleTableMD `shouldParseAs` simpleTable
      it "parses a valid full example table" $
        fullTableMD `shouldParseAs` fullTable
      it "doesn't parse for table without hr" $
        shouldNotParse tableWOBreakMD
      it "parses a table without column names" $
        tableWOColumnNamesMD `shouldParseAs` tableWOColumnNames
      it "parses a table without table name" $
        tableWOTableNameMD `shouldParseAs` tableWOTableName

    describe "rendering" $ do
      it "renders simple table with column names" $ do
        renderedT <- render simpleTable
        renderedT `shouldBe` T.toLazy simpleTableHtml
      it "renders simple table without column names" $ do
        renderedT <- render tableWOColumnNames
        renderedT `shouldBe` T.toLazy tableWOColumnNamesHtml
      it "renders full table example" $ do
        renderedT <- render fullTable
        renderedT `shouldBe` T.toLazy fullTableHtml

render :: MarkdownTable -> IO LText
render = liftIO . renderTextT . renderTable

stripPos :: MarkdownTable -> MarkdownTable
stripPos = transformBi (\(_ :: Maybe MD.PosInfo) -> Nothing)

-----------------------
------ Text Md --------
-----------------------

tableKeyword :: Text
tableKeyword = "+ %TABLE Table"

tableKeywordNoName :: Text
tableKeywordNoName = "+ %TABLE"

columnNamesList :: [Text]
columnNamesList =
  [ "+ - Column 1"
  , "  - Column 2"
  , "  - Column 3"
  ]

columnNamesSimple :: Text
columnNamesSimple = "+ Column 1|Column 2|Column 3"

breakText :: Text
breakText = "+ ----------------"

rowOfList :: [Text]
rowOfList =
  [ "+ - Foo"
  , "  - Bar"
  , "  - Baz"
  ]

rowWithSeparator :: Text
rowWithSeparator = "+ Another foo | Another bar | Another baz"

rowSeveralNodes :: Text
rowSeveralNodes = "+ *foo* | **bar** | baz"

rowOfListSeveralNodes :: [Text]
rowOfListSeveralNodes =
  [ "+ - ```"
  , "    Code foo"
  , "    ```"
  , "  - Simple bar"
  , "  - `inline code` baz"
  ]

---------------------------
------- MD elements -------
---------------------------

-- | “foo ” is parsed as @Text_ "foo"@ (without a space), but we want a
-- space there, so in the examples we use a dot instead of spaces.
putSpaces :: [[MD.Node]] -> [[MD.Node]]
putSpaces = transformBi $ \case
  MD.TEXT s -> MD.TEXT (T.replace "·" " " s)
  other     -> other

columnNames :: [[MD.Node]]
columnNames = map parseMD ["Column 1", "Column 2", "Column 3"]

rowOfListMD :: [[MD.Node]]
rowOfListMD = map parseMD ["Foo", "Bar", "Baz"]

rowSeveralNodesMD :: [[MD.Node]]
rowSeveralNodesMD =
  putSpaces $ map parseMD ["*foo*·", "·**bar**·", "·baz"]

rowWithSeparatorMD :: [[MD.Node]]
rowWithSeparatorMD =
  putSpaces $ map parseMD ["Another foo·", "·Another bar·", "·Another baz"]

rowOfListSeveralNodesMD :: [[MD.Node]]
rowOfListSeveralNodesMD =
  [ parseVanillaMD "```\nCode foo\n```"
  , parseMD "Simple bar"
  , parseMD "`inline code` baz"
  ]

----------------------------------------------------------------------------
-- HTML
----------------------------------------------------------------------------

tableNameHtml :: Text
tableNameHtml =
  "<h3>Table</h3>\
  \<table class=\"sortable\">"

columnNamesHtml :: Text
columnNamesHtml =
  "<thead><tr>\
      \<td><p>Column 1</p>\n</td>\
      \<td><p>Column 2</p>\n</td>\
      \<td><p>Column 3</p>\n</td>\
  \</tr></thead>"

bodyStart :: Text
bodyStart = "<tbody>"

rowOfListHtml :: Text
rowOfListHtml =
  "<tr>\
      \<td><p>Foo</p>\n</td>\
      \<td><p>Bar</p>\n</td>\
      \<td><p>Baz</p>\n</td>\
  \</tr>"

rowSeveralNodesHtml :: Text
rowSeveralNodesHtml =
  "<tr>\
      \<td><p><em>foo</em> </p>\n</td>\
      \<td><p> <strong>bar</strong> </p>\n</td>\
      \<td><p> baz</p>\n</td>\
  \</tr>"

rowWithSeparatorHtml :: Text
rowWithSeparatorHtml =
  "<tr>\
      \<td><p>Another foo </p>\n</td>\
      \<td><p> Another bar </p>\n</td>\
      \<td><p> Another baz</p>\n</td>\
  \</tr>"

rowOfListSeveralNodesHtml :: Text
rowOfListSeveralNodesHtml =
  "<tr>\
      \<td><pre><code>Code foo\n\
          \</code></pre>\n</td>\
      \<td><p>Simple bar</p>\n</td>\
      \<td><p><code>inline code</code> baz</p>\n</td>\
  \</tr>"

endHtml :: Text
endHtml = "</tbody></table>"

--------------------------------------

buildTableMD :: [Text] -> MD.Node
buildTableMD txtTable = head $ parseVanillaMD $ T.unlines txtTable

fullTableMD :: [Text]
fullTableMD =
  tableKeyword:columnNamesList ++
  breakText:rowSeveralNodes:rowOfList ++
  rowOfListSeveralNodes ++
  [rowWithSeparator]

simpleTableMD :: [Text]
simpleTableMD =
  tableKeyword:columnNamesSimple:breakText:rowOfList

simpleTableColListMD :: [Text]
simpleTableColListMD =
  tableKeyword:columnNamesList ++ breakText:rowOfList

tableWOBreakMD :: [Text]
tableWOBreakMD =
  tableKeyword:columnNamesSimple:rowOfList

tableWOColumnNamesMD :: [Text]
tableWOColumnNamesMD =
  tableKeyword:breakText:rowOfList

tableWOTableNameMD :: [Text]
tableWOTableNameMD =
  tableKeywordNoName:breakText:rowOfList

buildTable :: Maybe Text -> Maybe [[MD.Node]] -> [[[MD.Node]]] -> MarkdownTable
buildTable tabNm colNm rows =
  MarkdownTable { markdownTableName = tabNm
                , markdownTableColumns = colNm
                , markdownTableRows = rows
                }

fullTable :: MarkdownTable
fullTable =
  buildTable (Just "Table") (Just columnNames)
    [ rowSeveralNodesMD
    , rowOfListMD
    , rowOfListSeveralNodesMD
    , rowWithSeparatorMD
    ]

simpleTable :: MarkdownTable
simpleTable = buildTable (Just "Table") (Just columnNames) [rowOfListMD]

tableWOColumnNames :: MarkdownTable
tableWOColumnNames = buildTable (Just "Table") Nothing [rowOfListMD]

tableWOTableName :: MarkdownTable
tableWOTableName = buildTable Nothing Nothing [rowOfListMD]

buildTableHtml :: Text -> [Text] -> Text
buildTableHtml cols rows = T.concat $ tableNameHtml:cols:bodyStart:rows ++ [endHtml]

simpleTableHtml :: Text
simpleTableHtml = buildTableHtml columnNamesHtml [rowOfListHtml]

tableWOColumnNamesHtml :: Text
tableWOColumnNamesHtml = buildTableHtml "" [rowOfListHtml]

fullTableHtml :: Text
fullTableHtml =
  buildTableHtml columnNamesHtml [ rowSeveralNodesHtml
                                 , rowOfListHtml
                                 , rowOfListSeveralNodesHtml
                                 , rowWithSeparatorHtml]
