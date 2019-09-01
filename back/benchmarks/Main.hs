-- | Benchmarks to check time of jsonb query.
module Main
       (
         main
       ) where

import Imports

import Gauge
import Hasql.Transaction.Sessions (Mode (..))
import Hasql.Connection (Connection)

import Guide.Database.Queries.Update
import Guide.Database.Queries.Select
import Guide.Types.Core
import Guide.Database.Connection


main :: IO ()
main = do
  conn <- connect
  defaultMain [databaseBenchmark conn]

databaseBenchmark :: Connection -> Benchmark
databaseBenchmark conn =
  let update :: Category -> Category
      update =  _categoryTitle <>~ " +"
  in bgroup "Database"
      [ bench "select" $ nfIO $
          runTransactionExceptT conn Read $ selectCategory "category1111"
      , bench "update" $ nfIO $
          runTransactionExceptT conn Write $ updateCategory "category1111" update
      ]

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
