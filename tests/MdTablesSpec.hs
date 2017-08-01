{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MdTablesSpec (tests) where

import           BasePrelude
import           Control.Monad.IO.Class (liftIO)
-- CMark
import qualified CMark                  as MD
-- Text
import           Data.Text.All          (Text)
import qualified Data.Text.All          as T
-- Testing
import           Test.Hspec
-- Lucid
import           Lucid.Base             (renderTextT)

import           Guide.Markdown

tests :: Spec
tests =
  describe "MdTables.table" $ do
    describe "getting" $ do
      it "returns simple table with column names from list" $
        getTable simpleTableColListMD `shouldBe` Just simpleTable
      it "returns simple table with column names from text with separator" $
        getTable simpleTableMD `shouldBe` Just simpleTable
      it "returns valid full example table" $
        getTable fullTableMD `shouldBe` Just fullTable
      it "returns nothing for table without hr" $
        getTable tableWOBreakMD `shouldBe` Nothing
      it "returns table without column names" $
        getTable tableWOColumnNamesMD `shouldBe` Just tableWOColumnNames
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

render = liftIO . renderTextT . renderTable

-----------------------
------ Text Md --------
-----------------------

tableKeyword :: Text
tableKeyword = "+ %TABLE Table"

columnNamesList :: [Text]
columnNamesList =
  [ "+ - Column 1"
  , "  - Column 2"
  , "  - Column 3"
  ]

columnNamesSimple :: Text
columnNamesSimple = "+ - Column 1|Column 2|Column 3"

breakText :: Text
breakText = "+ ----------------"

rowOfList :: [Text]
rowOfList =
  [ "+ - Foo"
  , "  - Bar"
  , "  - Baz"
  , ""
  ]

rowWithSeparator :: Text
rowWithSeparator = "+ - Another foo | Another bar | Another baz"

rowSeveralNodes :: Text
rowSeveralNodes = "+ - *foo* | **bar** | baz"

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

columnNames :: [[MD.Node]]
columnNames = [ [ MD.Node Nothing (MD.TEXT "Column 1") [] ]
              , [ MD.Node Nothing (MD.TEXT "Column 2") [] ]
              , [ MD.Node Nothing (MD.TEXT "Column 3") [] ]
              ]

rowOfListMD :: [[MD.Node]]
rowOfListMD = [ [ MD.Node Nothing (MD.TEXT "Foo") [] ]
              , [ MD.Node Nothing (MD.TEXT "Bar") [] ]
              , [ MD.Node Nothing (MD.TEXT "Baz") [] ]
              ]

rowSeveralNodesMD :: [[MD.Node]]
rowSeveralNodesMD = [ [ MD.Node Nothing MD.EMPH [ MD.Node Nothing (MD.TEXT "foo") [] ]
                      , MD.Node Nothing (MD.TEXT " ") []
                      ]
                    , [ MD.Node Nothing (MD.TEXT " ") []
                      , MD.Node Nothing MD.STRONG [ MD.Node Nothing (MD.TEXT "bar") [] ]
                      , MD.Node Nothing (MD.TEXT " ") []
                      ]
                    , [ MD.Node Nothing (MD.TEXT " baz") [] ]
                    ]

rowWithSeparatorMD :: [[MD.Node]]
rowWithSeparatorMD = [ [ MD.Node Nothing (MD.TEXT "Another foo ") [] ]
                     , [ MD.Node Nothing (MD.TEXT " Another bar ") [] ]
                     , [ MD.Node Nothing (MD.TEXT " Another baz") [] ]
                     ]

rowOfListSeveralNodesMD :: Int -> [[MD.Node]]
rowOfListSeveralNodesMD n =
  [ [ MD.Node ( Just
                MD.PosInfo
              { startLine = n
              , startColumn = 5
              , endLine = n + 2
              , endColumn = 7
              })
        (MD.HTML_BLOCK
           "<div class=\"sourceCode\"><pre class=\"sourceCode\"><code class=\"sourceCode\">Code foo</code></pre></div>")
        []
    ]
  , [ MD.Node Nothing (MD.TEXT "Simple bar") [] ]
  , [ MD.Node Nothing (MD.CODE "inline code") []
    ,  MD.Node Nothing (MD.TEXT " baz") []
    ]
  ]

-----------------
----- HTML ------
-----------------
tableNameHtml :: Text
tableNameHtml = "<h3>Table</h3><table class=\"sortable\">"

columnNamesHtml :: Text
columnNamesHtml =
  "<thead><tr><td>Column 1</td><td>Column 2</td><td>Column 3</td></tr></thead>"

bodyStart :: Text
bodyStart = "<tbody>"

rowOfListHtml :: Text
rowOfListHtml =
  "<tr><td>Foo</td><td>Bar</td><td>Baz</td></tr>"

rowSeveralNodesHtml :: Text
rowSeveralNodesHtml =
  "<tr><td>&lt;em&gt;foo&lt;/em&gt; </td><td> &lt;strong&gt;bar&lt;/strong&gt; </td><td> baz</td></tr>"

rowWithSeparatorHtml :: Text
rowWithSeparatorHtml =
  "<tr><td>Another foo </td><td> Another bar </td><td> Another baz</td></tr>"

rowOfListSeveralNodesHtml :: Text
rowOfListSeveralNodesHtml =
  "<tr><td>&lt;div class=&quot;sourceCode&quot;&gt;&lt;pre class=&quot;sourceCode&quot;&gt;&lt;code class=&quot;sourceCode&quot;&gt;Code foo&lt;/code&gt;&lt;/pre&gt;&lt;/div&gt;\n</td><td>Simple bar</td><td>&lt;code&gt;inline code&lt;/code&gt; baz</td></tr>"

endHtml :: Text
endHtml = "</tbody></table>"

--------------------------------------

buildTableMD :: [Text] -> MD.Node
buildTableMD txtTable = head $ parseMD $ T.unlines txtTable

fullTableMD :: MD.Node
fullTableMD = buildTableMD
  (  tableKeyword:columnNamesList
  ++ breakText:rowSeveralNodes:rowOfList
  ++ rowOfListSeveralNodes
  ++ [rowWithSeparator]
  )

simpleTableMD :: MD.Node
simpleTableMD =
  buildTableMD (tableKeyword:columnNamesSimple:breakText:rowOfList)

simpleTableColListMD :: MD.Node
simpleTableColListMD =
  buildTableMD (tableKeyword:columnNamesList ++ breakText:rowOfList)

tableWOBreakMD :: MD.Node
tableWOBreakMD =
  buildTableMD (tableKeyword:columnNamesSimple:rowOfList)

tableWOColumnNamesMD :: MD.Node
tableWOColumnNamesMD =
  buildTableMD (tableKeyword:breakText:rowOfList)

buildTable :: Maybe [[MD.Node]] -> [[[MD.Node]]] -> MarkdownTable
buildTable colNm rows =
  MarkdownTable { markdownTableName = "Table"
                , markdownTableColumns = colNm
                , markdownTableRows = rows
                }

fullTable :: MarkdownTable
fullTable =
  buildTable (Just columnNames) [ rowSeveralNodesMD
                                , rowOfListMD
                                , rowOfListSeveralNodesMD 11
                                , rowWithSeparatorMD
                                ]

simpleTable :: MarkdownTable
simpleTable = buildTable (Just columnNames) [rowOfListMD]

tableWOColumnNames :: MarkdownTable
tableWOColumnNames = buildTable Nothing [rowOfListMD]

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
