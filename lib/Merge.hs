{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Merge
(
  merge,
)
where


import BasePrelude
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
import Data.List.Split
-- Vector
import qualified Data.Vector as V
-- Diffing
import qualified Data.Patch as PV


-- | An implementation of a 3-way diff and merge.
merge
  :: Text    -- ^ Original text
  -> Text    -- ^ Variant A (preferred)
  -> Text    -- ^ Variant B
  -> Text    -- ^ Merged text
merge orig a b = T.concat . V.toList $ PV.apply (pa <> pb') orig'
  where
    (orig', a', b') = (orig, a, b) & each %~
      V.fromList . consolidate . map T.toStrict . break' . T.toString
    pa = PV.diff orig' a'
    pb = PV.diff orig' b'
    (_, pb') = PV.transformWith PV.ours pa pb

-- | Break a string into words, spaces, and special characters.
break' :: String -> [String]
break' = split . dropInitBlank . dropFinalBlank . dropInnerBlanks . whenElt $
  \c -> not (isAlphaNum c) && c /= '\''

-- | Consolidate some of the things into tokens (like links, consecutive
-- spaces, and Markdown elements).
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
-- the rest
consolidate (x:xs) = x : consolidate xs
consolidate [] = []
