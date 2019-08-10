-- | The code that powers the searchbox.
module Guide.Search
(
  SearchResult(..),
  search,
)
where


import Imports

import Guide.Markdown
import Guide.State
import Guide.Types

import qualified Data.Set as S
import qualified Data.Text as T


-- | A search result.
data SearchResult
  -- | Category's title matches the query
  = SRCategory Category
  -- | Item's name matches the query
  | SRItem Category Item
  -- | Item's ecosystem matches the query
  | SRItemEcosystem Category Item
  deriving (Show, Generic)

-- | Find things matching a simple text query, and return results ranked by
-- importance. Categories are considered more important than items.
--
-- Currently 'search' doesn't do any fuzzy search whatsoever – only direct
-- word matches are considered. See 'match' for the description of the
-- matching algorithm.
search :: Text -> GlobalState -> [SearchResult]
search query gs =
    -- category titles
    sortByRank [(SRCategory cat, rank)
                 | cat <- categories gs
                 , let rank = match query (categoryTitle cat)
                 , rank > 0 ] ++
    -- item names
    sortByRank [(SRItem cat item, rank)
                 | cat  <- categories gs
                 , item <- categoryItems cat
                 , let rank = match query (itemName item)
                 , rank > 0 ] ++
    -- item ecosystems
    sortByRank [(SRItemEcosystem cat item, rank)
                 | cat  <- categories gs
                 , item <- categoryItems cat
                 , let rank = match query (markdownBlockSource (itemEcosystem item))
                 , rank > 0 ]
  where
    sortByRank :: [(a, Int)] -> [a]
    sortByRank = map fst . sortOn (Down . snd)

-- | How many words in two strings match?
--
-- Words are defined as sequences of letters, digits and characters like
-- “-”; separators are everything else. Comparisons are case-insensitive.
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
