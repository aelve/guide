{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module MdTablesSpec (tests) where

import BasePrelude
-- CMark
import qualified CMark as MD
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Testing
import Test.Hspec

import Guide.Markdown

tests :: Spec
tests =
  describe "MdTables.table" $
    describe "getting" $ do
      it "returns valid full example table" $
        getTable fullTableMD `shouldBe` (Just $ fullTable (Just columnNames) 12)
      it "returns nothing for table without hr" $
        getTable tableWOBreak `shouldBe` Nothing
      it "returns table without column names" $
        getTable tableWOColumnNames `shouldBe` (Just $ fullTable Nothing 9)


textTable :: [Text]
textTable =
  [ "+ %TABLE Table"
  , "+ - Column 1"
  , "  - Column 2"
  , "  - Column 3"
  , "+ ----------------"
  , "+ - *foo* | **bar** | baz"
  , ""
  , "+ - Foo"
  , "  - Bar"
  , "  - Baz"
  , ""
  , "+ - ```"
  , "    Code foo"
  , "    ```"
  , "  - Simple bar"
  , "  - `inline code` baz"
  , "+ - Another foo | Another bar | Another baz"
  ]

buildTable :: [Text] -> MD.Node
buildTable txtTable = head $ parseMD $ T.unlines txtTable

fullTableMD :: MD.Node
fullTableMD = buildTable textTable

tableWOBreak :: MD.Node
tableWOBreak = buildTable $ take 4 textTable ++ drop 5 textTable

tableWOColumnNames :: MD.Node
tableWOColumnNames = buildTable $ take 1 textTable ++ drop 4 textTable

columnNames :: [[MD.Node]]
columnNames = [ [ MD.Node Nothing (MD.TEXT "Column 1") [] ]
              , [ MD.Node Nothing (MD.TEXT "Column 2") [] ]
              , [ MD.Node Nothing (MD.TEXT "Column 3") [] ]
              ]
fullTable :: Maybe [[MD.Node]] -> Int -> MarkdownTable
fullTable colNm n = MarkdownTable { markdownTableName = "Table"
                      , markdownTableColumns = colNm
                      , markdownTableRows =
                          [ [ [ MD.Node Nothing MD.EMPH [ MD.Node Nothing (MD.TEXT "foo") [] ]
                              , MD.Node Nothing (MD.TEXT " ") []
                              ]
                            , [ MD.Node Nothing (MD.TEXT " ") []
                              , MD.Node Nothing MD.STRONG [ MD.Node Nothing (MD.TEXT "bar") [] ]
                              , MD.Node Nothing (MD.TEXT " ") []
                              ]
                            , [ MD.Node Nothing (MD.TEXT " baz") [] ]
                            ]
                          , [ [ MD.Node Nothing (MD.TEXT "Foo") [] ]
                            , [ MD.Node Nothing (MD.TEXT "Bar") [] ]
                            , [ MD.Node Nothing (MD.TEXT "Baz") [] ]
                            ]
                          , [ [ MD.Node ( Just
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
                          , [ [ MD.Node Nothing (MD.TEXT "Another foo ") [] ]
                            , [ MD.Node Nothing (MD.TEXT " Another bar ") [] ]
                            , [ MD.Node Nothing (MD.TEXT " Another baz") [] ]
                            ]
                          ]
                      }

