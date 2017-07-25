{- |
  Code Snippets Parser
-}
module Snippets.Parser
( mainParse
, SnippetNode(..)
)
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M (fromList)
import Imports

-- Text
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
-- MegaParsec
import           Text.Megaparsec      ( alphaNumChar, anyChar
                                      , between, char
                                      , letterChar, many
                                      , manyTill, notFollowedBy
                                      , sepBy, space, string
                                      )
import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Text (Parser)


data SnippetNode = Multiple (IntMap Text)
                 | Choice (Map Text Text)
                 | CodeText Text
                 | Hackage Text
                 | HltBegin
                 | HltEnd
                 | HltLine
                 deriving (Show, Eq)

sumSnippetNodes :: [SnippetNode] -> [SnippetNode]
sumSnippetNodes [] = []
sumSnippetNodes (CodeText x:(CodeText y:other)) = sumSnippetNodes ((CodeText $ x <> y):other)
sumSnippetNodes (x:xs) = x: sumSnippetNodes xs

parseLine :: Parser [SnippetNode]
parseLine = MP.try parseMultiple <|> parseSimpleLine

parseSimpleLine :: Parser [SnippetNode]
parseSimpleLine = many parseSimpleLinePiece >>= pure . sumSnippetNodes

parseSimpleLinePiece :: Parser SnippetNode
parseSimpleLinePiece =  MP.try parseHltLine
                    <|> MP.try parseChoice
                    <|> MP.try parseHackage
                    <|> MP.try parseHltBegin
                    <|> parseHltEnd
                    <|> (anyChar >>= \x -> pure $ CodeText $ T.pack [x])
-----------------------------------------------


text :: String -> Parser Text
text t = T.pack <$> string t


txtP :: Parser Text
txtP = T.pack <$> (space *> char '"' *> anyChar `manyTill` char '"' <* space)

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (char '[' ) (char ']')

keyword :: String -> Parser ()
keyword w = between (text "{{") (text "}}") $  space <* text w *> notFollowedBy alphaNumChar *> space

labels :: Parser [Text]
labels = txtP `sepBy` char ','

choiceP :: Parser [(Text, Text)]
choiceP = labels <* char ':' >>= \lbls -> txtP >>= \val -> pure $ fromChoicesToMap lbls val

fromChoicesToMap :: [Text] -> Text -> [(Text, Text)]
fromChoicesToMap lbls val = (\x -> (x, val)) <$> lbls

parseMultiple :: Parser [SnippetNode]
parseMultiple = do
  keyword "Multiple"
  multNames <- betweenBrackets labels
  pure [Multiple $ IM.fromList $ zip [1..] multNames]

parseChoice :: Parser SnippetNode
parseChoice = do
  keyword "Choice"
  multNames <- betweenBrackets (choiceP `sepBy` char ';')
  pure $ Choice $ M.fromList $ concat multNames

parseHackage :: Parser SnippetNode
parseHackage = do
  keyword "Hackage"
  name <- betweenBrackets (many $ letterChar <|> char '.')
  pure $ Hackage $ T.pack name

parseHltBegin :: Parser SnippetNode
parseHltBegin = keyword "HltBegin" >> pure HltBegin

parseHltEnd :: Parser SnippetNode
parseHltEnd = keyword "HltEnd" >> pure HltEnd

parseHltLine :: Parser SnippetNode
parseHltLine = keyword "HltLine" >> pure HltLine

mainParse :: IO [[SnippetNode]]
mainParse = do
  prog <- TIO.readFile "prog.txt"
  let progLines = T.lines prog
  for progLines $ \line -> do
    let nodes = MP.parse parseLine "" line
    case nodes of
          Left err -> pure [CodeText (T.pack $ show err)]
          Right p  -> pure p
