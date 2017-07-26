{-# LANGUAGE TupleSections #-}
{-|
  Code Snippets Parser
-}
module Snippets.Parser
( mainParse
, Snippet
, SnippetLine
, SnippetNode(..)
)
where

import qualified Data.Map             as M (fromList)
import           Imports

-- Text
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
-- MegaParsec
import           Text.Megaparsec      (alphaNumChar, anyChar, between, char,
                                       letterChar, many, manyTill,
                                       notFollowedBy, sepBy, space, string)
import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Text (Parser)

{-|
Result of parsing.
Snippet is represented by list of parsed code lines.
-}
type Snippet = [SnippetLine]

{-|
Result of line parsing.
SnippetLine is represented by list of parsed nodes of the text line.
-}
type SnippetLine = [SnippetNode]

{-|
Acceptable options for code manipulations in snippets.
-}
data SnippetNode
  -- | In the beginning of the snippet creates number of labels,
  -- so chain snippets can be build using 'Choice' constructor later in code
  = Multiple [(Int, Text)]
  -- | Creates multiple options for code text depends on
  -- specified label, which is created with 'Multiple'
  | Choice (Map Text Text)
  -- | Simple code block
  | CodeText Text
  -- | Links to package at [hackage](http://hackage.haskell.org/)
  | Hackage Text
  -- | Start point for inline code highlighting
  | HltBegin
  -- | End point for inline code highlighting
  | HltEnd
  -- | Whole line highlighting
  | HltLine
  deriving (Show, Eq)

-- As I haven't found still better option for parsing except symb by symb parsing
-- lots of CodeTexts of 1 char needs to be combined in CodeText blocks
sumSnippetNodes :: SnippetLine -> SnippetLine
sumSnippetNodes [] = []
sumSnippetNodes (CodeText x:(CodeText y:other)) = sumSnippetNodes ((CodeText $ x <> y):other)
sumSnippetNodes (x:xs) = x: sumSnippetNodes xs

-- first line in the snippet could be either just Multiple labels or any other simple line
parseLine :: Parser SnippetLine
parseLine = MP.try parseMultiple <|> parseSimpleLine

parseSimpleLine :: Parser SnippetLine
parseSimpleLine = many parseSimpleLinePiece >>= pure . sumSnippetNodes

parseSimpleLinePiece :: Parser SnippetNode
parseSimpleLinePiece =  MP.try parseHltLine
                    <|> MP.try parseChoice
                    <|> MP.try parseHackage
                    <|> MP.try parseHltBegin
                    <|> parseHltEnd
                    <|> (anyChar >>= \x -> pure $ CodeText $ T.pack [x]) -- TODO: how to optimize? manyTill doesn't work :(

-----------------------------------------------

text :: String -> Parser Text
text t = T.pack <$> string t

txtP :: Parser Text
txtP = T.pack <$> (space *> char '"' *> anyChar `manyTill` char '"' <* space)

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (char '[' ) (char ']')

-- keywords should be single word inside double parentheses like {{Multiple}}
keyword :: String -> Parser ()
keyword w = between (text "{{") (text "}}") $  space <* text w *> notFollowedBy alphaNumChar *> space

labels :: Parser [Text]
labels = txtP `sepBy` char ','

{-|
Assume before was {{Multiple}}["Code1", "Code2", "Code3", "Code4"]
@
 {{Choice}}["Code1", "Code2" : "product";  "Code4" : "foldl' 1 (*)"]
@
should be generated to
@
 [("Code1", "product"), ("Code2", "product"), ("Code4", "foldl' 1 (*)")]
@

 -}
choiceP :: Parser [(Text, Text)]
choiceP = liftA2 fromChoicesToMap (labels <* char ':') txtP

fromChoicesToMap :: [Text] -> Text -> [(Text, Text)]
fromChoicesToMap lbls val = (, val) <$> lbls

{-|
Labels could be created like
@
{{Multiple}}["Code1", "Code2", "Code3", "Code4"]
@
-}
parseMultiple :: Parser SnippetLine
parseMultiple = do
  keyword "Multiple"
  multNames <- betweenBrackets labels
  pure [Multiple $ zip [1..] multNames]

{-|
If labels are used in snippets you can use 'Choice' for different code examples in one
@
factorial :: Int -> {{Choice}}["Code1" : "Int"; "Code2": "Integer"]
@
means that 2 options of snippet will be created
Code1:
@
factorial :: Int -> Int
@

Code2:
@
factorial :: Int -> Integer
@
-}
parseChoice :: Parser SnippetNode
parseChoice = do
  keyword "Choice"
  multNames <- betweenBrackets (choiceP `sepBy` char ';')
  pure $ Choice $ M.fromList $ concat multNames

{-|
Links to packages at hackage will be attached:
@
import {{Hackage}}[Data.Monoid]
@
-}
parseHackage :: Parser SnippetNode
parseHackage = do
  keyword "Hackage"
  name <- betweenBrackets (many $ letterChar <|> char '.') -- TODO: possibly incorrect, think later
  pure $ Hackage $ T.pack name

{-|
Highlighting any part inside the code line
@
factorial :: Int -> {{HltBegin}}Integer{{HltEnd}}
@
-}
parseHltBegin :: Parser SnippetNode
parseHltBegin = keyword "HltBegin" >> pure HltBegin

parseHltEnd :: Parser SnippetNode
parseHltEnd = keyword "HltEnd" >> pure HltEnd

{-|
Highlighting the whole line of code
@
factorial :: Int -> Integer
{{HltLine}}factorial n = foldl' 1 (*) [1..fromIntegral n]
@
-}
parseHltLine :: Parser SnippetNode
parseHltLine = keyword "HltLine" >> pure HltLine

-- maybe better data structure for parse result
-- data ParsedSnippet = MultipleBlocks [(Int,Text)] [[SnippetNode]]
--                    | SingleBlock                 [[SnippetNode]]

-- parsing function for testing on 'admin/snippets'
mainParse :: IO Snippet
mainParse = do
  prog <- TIO.readFile "tests/SnippetsExample.md"
  let progLines = T.lines prog
  for progLines $ \line -> do
    let nodes = MP.parse parseLine "" line
    case nodes of
          Left err -> pure [CodeText (T.pack $ show err)]
          Right p  -> pure p
