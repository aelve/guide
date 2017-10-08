-- | The code that powers the searchbox.
module Guide.Search
(
  SearchResult(..),
  search,
)
where


import Imports

-- Text
import qualified Data.Text.All as T
-- Sets
import qualified Data.Set as S

import Guide.Types
import Guide.State
import Guide.Markdown


-- | A search result.
data SearchResult
  -- | Category's title matches the query
  = SRCategory Category
  -- | Item's name matches the query
  | SRItem Category Item
  -- | Item's ecosystem matches the query
  | SRItemEcosystem Category Item
  deriving (Show, Generic)

{- | Find things matching a simple text query, and return results ranked by
importance. Categories are considered more important than items.

Currently 'search' doesn't do any fuzzy search whatsoever – only direct word
matches are considered. See 'match' for the description of the matching
algorithm.
-}
search :: Text -> GlobalState -> [SearchResult]
search query gs =
    -- category titles
    sortByRank [(SRCategory cat, rank)
                 | cat <- gs^.categories
                 , let rank = match query (cat^.title)
                 , rank > 0 ] ++
    -- item names
    sortByRank [(SRItem cat item, rank)
                 | cat  <- gs^.categories
                 , item <- cat^.items
                 , let rank = match query (item^.name)
                 , rank > 0 ] ++
    -- item ecosystems
    sortByRank [(SRItemEcosystem cat item, rank)
                 | cat  <- gs^.categories
                 , item <- cat^.items
                 , let rank = match query (item^.ecosystem.mdSource)
                 , rank > 0 ]
  where
    sortByRank :: [(a, Int)] -> [a]
    sortByRank = map fst . sortOn (Down . snd)

{- | How many words in two strings match?

Words are defined as sequences of letters, digits and characters like “-”;
separators are everything else. Comparisons are case-insensitive.
-}
match :: Text -> Text -> Int
match a b = common (getWords a) (getWords b)
  where
    isWordPart c = isLetter c || isDigit c || c == '-'
    getWords =
      map T.toTitle .
      filter (not . T.null) .
      T.split (not . isWordPart)

-- | Find how many elements two lists have in common.
common :: Ord a => [a] -> [a] -> Int
common a b = S.size (S.intersection (S.fromList a) (S.fromList b))
