{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}


-- | Prepare text for diffing or merging by breaking it into tokens (like
-- links or Markdown elements).
module Guide.Diff.Tokenize
(
  tokenize,
)
where


import Imports

-- Text
import qualified Data.Text.All as T
import Data.List.Split
-- Vector
import qualified Data.Vector as V
import Data.Vector (Vector)


-- | Break text into tokens.
tokenize :: Text -> Vector Text
tokenize = V.fromList . consolidate . map T.toStrict . break' . T.toString

-- | Break a string into words, spaces, and special characters.
break' :: String -> [String]
break' = split . dropInitBlank . dropFinalBlank . dropInnerBlanks . whenElt $
  \c -> not (isAlphaNum c) && c /= '\''

-- | Consolidate some of the things into tokens.
consolidate :: [Text] -> [Text]
-- spaces
consolidate s@(" ":_) =
  let (l, r) = span (== " ") s
  in T.concat l : consolidate r
-- breaks between paragraphs
consolidate s@("\n":_) =
  let (l, r) = span (== "\n") s
  in T.concat l : consolidate r
-- code block markers
consolidate s@("~":_) =
  let (l, r) = span (== "~") s
  in if length l >= 3 then T.concat l : consolidate r else l ++ consolidate r
consolidate s@("`":_) =
  let (l, r) = span (== "`") s
  in if length l >= 3 then T.concat l : consolidate r else l ++ consolidate r
-- hrules
consolidate s@("-":_) =
  let (l, r) = span (== "-") s
  in if length l >= 3 then T.concat l : consolidate r else l ++ consolidate r
-- ellipses
consolidate (".":".":".":xs) = "..." : consolidate xs
-- links
consolidate s@("http":":":"/":"/":_) =
  let (l, r) = span (\x -> x /= ")" && not (isSpace (T.head x))) s
  in  T.concat l : consolidate r
consolidate s@("https":":":"/":"/":_) =
  let (l, r) = span (\x -> x /= ")" && not (isSpace (T.head x))) s
  in  T.concat l : consolidate r
consolidate ("(":"@":"hk":")":xs) = "(" : "@hk" : ")" : consolidate xs

-- Haskell tokens
consolidate (":":":":xs) = "::" : consolidate xs
consolidate (".":".":xs) = ".." : consolidate xs
consolidate ("[":"]":xs) = "[]" : consolidate xs
consolidate ("(":")":xs) = "()" : consolidate xs
consolidate ("[":"|":xs) = "[|" : consolidate xs
consolidate ("|":"]":xs) = "|]" : consolidate xs
-- Haskell operators
consolidate (op -> ("++" , xs)) = "++"  : consolidate xs
consolidate (op -> ("<>" , xs)) = "<>"  : consolidate xs
consolidate (op -> ("!!" , xs)) = "!!"  : consolidate xs
consolidate (op -> (">>" , xs)) = ">>"  : consolidate xs
consolidate (op -> ("&&" , xs)) = "&&"  : consolidate xs
consolidate (op -> ("||" , xs)) = "||"  : consolidate xs
consolidate (op -> ("<$>", xs)) = "<$>" : consolidate xs
consolidate (op -> ("<*>", xs)) = "<*>" : consolidate xs
consolidate (op -> (">>=", xs)) = ">>=" : consolidate xs
consolidate (op -> ("=<<", xs)) = "=<<" : consolidate xs

-- the rest
consolidate (x:xs) = x : consolidate xs
consolidate [] = []

-- | Helpful view pattern for matching operators
op :: [Text] -> (Text, [Text])
op = over _1 mconcat . span (isOpToken . T.unpack)
  where
    isOpToken [c] = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String)
    isOpToken _   = False
