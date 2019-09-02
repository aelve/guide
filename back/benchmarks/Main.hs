-- | Benchmarks to check time of jsonb query.
module Main
       (
         main
       ) where

import Imports

import Gauge
import Hasql.Transaction.Sessions (Mode (..))
import Hasql.Connection (Connection)
import qualified Data.Set as Set

import Guide.Database.Queries.Update
import Guide.Database.Queries.Select
import Guide.Database.Queries.Insert
import Guide.Database.Queries.Delete
import Guide.Types.Core
import Guide.Database.Connection
import Guide.Markdown (toMarkdownBlock, toMarkdownTree)


-- | See readme for instruction.
main :: IO ()
main = do
  conn <- connect
  time <- getCurrentTime
  runTransactionExceptT conn Write $
    insertCategoryWithCategory (#archived False) $ category time
  defaultMain [databaseBenchmark conn]
  runTransactionExceptT conn Write $ deleteCategory "categoryUid1"

databaseBenchmark :: Connection -> Benchmark
databaseBenchmark conn =
  let update :: Category -> Category
      update =  _categoryTitle <>~ " +"
  in bgroup "Database"
      [ bench "select" $ nfIO $
          runTransactionExceptT conn Read $ selectCategory "categoryUid1"
      , bench "update" $ nfIO $
          runTransactionExceptT conn Write $ updateCategory "categoryUid1" update
      ]

category :: UTCTime -> Category
category time = Category
  { categoryUid = "categoryUid1"
  , categoryTitle = "catTitle1"
  , categoryCreated =  time
  , categoryGroup = "group1"
  , categoryStatus = CategoryStub
  , categoryNotes = toMarkdownBlock "notes"
  , categoryItems = [item time]
  , categoryItemsDeleted = []
  , categoryEnabledSections = Set.fromList []
  }

item :: UTCTime -> Item
item time = Item
  { itemUid = "itemUid1234"
  , itemName = "title"
  , itemCreated = time
  , itemHackage = Just "hello"
  , itemSummary = toMarkdownBlock "summary"
  , itemPros = []
  , itemProsDeleted = []
  , itemCons = []
  , itemConsDeleted = []
  , itemEcosystem = toMarkdownBlock "eco"
  , itemNotes = toMarkdownTree "" "notes"
  , itemLink = Just "google.ru"
  }

{-
benchmarked Database/select
time                 843.9 μs   (812.4 μs .. 879.6 μs)
                     0.987 R²   (0.978 R² .. 0.993 R²)
mean                 846.1 μs   (823.0 μs .. 864.6 μs)
std dev              69.75 μs   (58.01 μs .. 87.73 μs)
variance introduced by outliers: 53% (severely inflated)

benchmarked Database/update
time                 2.435 ms   (2.388 ms .. 2.480 ms)
                     0.995 R²   (0.992 R² .. 0.998 R²)
mean                 2.358 ms   (2.313 ms .. 2.396 ms)
std dev              148.0 μs   (117.9 μs .. 194.6 μs)
variance introduced by outliers: 39% (moderately inflated)
-}
