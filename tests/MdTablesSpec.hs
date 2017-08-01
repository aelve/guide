{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MdTablesSpec (tests) where

import           BasePrelude
-- CMark
import qualified CMark          as MD
-- Text
import           Data.Text.All  (Text)
import qualified Data.Text.All  as T
-- Testing
import           Test.Hspec

import           Guide.Markdown

tests :: Spec
tests =
  describe "MdTables.table" $
    describe "getting" $ do
      it "return simple table with column names from list" $
        getTable simpleTableColListMD `shouldBe` Just simpleTable
      it "return simple table with column names from text with separator" $
        getTable simpleTableMD `shouldBe` Just simpleTable
      it "returns valid full example table" $
        getTable fullTableMD `shouldBe` Just fullTable
      it "returns nothing for table without hr" $
        getTable tableWOBreakMD `shouldBe` Nothing
      it "returns table without column names" $
        getTable tableWOColumnNamesMD `shouldBe` Just tableWOColumnNames

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

--------------------------------------

buildTableMD :: [Text] -> MD.Node
buildTableMD txtTable = head $ parseMD $ T.unlines txtTable

fullTableMD :: MD.Node
fullTableMD = buildTableMD
  (tableKeyword:columnNamesList ++ breakText:rowSeveralNodes:rowOfList ++ rowOfListSeveralNodes ++ [rowWithSeparator])

simpleTableMD :: MD.Node
simpleTableMD = buildTableMD (tableKeyword:columnNamesSimple:breakText:rowOfList)

simpleTableColListMD :: MD.Node
simpleTableColListMD = buildTableMD (tableKeyword:columnNamesList ++ breakText:rowOfList)

tableWOBreakMD :: MD.Node
tableWOBreakMD = buildTableMD (tableKeyword:columnNamesSimple:rowOfList)

tableWOColumnNamesMD :: MD.Node
tableWOColumnNamesMD = buildTableMD (tableKeyword:breakText:rowOfList)

buildTable :: Maybe [[MD.Node]] -> [[[MD.Node]]] -> MarkdownTable
buildTable colNm rows =
  MarkdownTable { markdownTableName = "Table"
                , markdownTableColumns = colNm
                , markdownTableRows = rows
                }

fullTable :: MarkdownTable
fullTable = buildTable (Just columnNames) [ rowSeveralNodesMD
                                     , rowOfListMD
                                     , rowOfListSeveralNodesMD 12
                                     , rowWithSeparatorMD
                                     ]

simpleTable :: MarkdownTable
simpleTable = buildTable (Just columnNames) [rowOfListMD]

tableWOColumnNames :: MarkdownTable
tableWOColumnNames = buildTable Nothing [rowOfListMD]
