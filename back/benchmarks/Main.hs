-- | Module contains all stuff to migrate from AcidState to Postgres.
module Main
       (
         main
       ) where

import Imports

import Gauge
import Hasql.Transaction.Sessions (Mode (..))

import Guide.Database.Queries.Update
import Guide.Database.Queries.Select
import Guide.Types.Core
import Guide.Database.Connection


main :: IO ()
main = do
    conn <- connect
    defaultMain [databaseBenchmark conn]
  where
    databaseBenchmark conn =
      bgroup
        "Database"
        [ bench "select" $ nfIO $
            runTransactionExceptT conn Read $ selectCategory "category1111"
        , bench "updete" $ nfIO $
            runTransactionExceptT conn Write $ updateCategory "category1111" update
        ]
    update :: Category -> Category
    update = _categoryTitle .~ "title10"

{-
benchmarked Database/select
time                 496.1 μs   (429.1 μs .. 551.4 μs)
                     0.932 R²   (0.868 R² .. 0.976 R²)
mean                 590.7 μs   (502.5 μs .. 939.6 μs)
std dev              508.6 μs   (51.75 μs .. 1.065 ms)
variance introduced by outliers: 97% (severely inflated)

benchmarked Database/updete
time                 497.4 μs   (429.4 μs .. 542.0 μs)
                     0.900 R²   (0.825 R² .. 0.948 R²)
mean                 1.429 ms   (520.2 μs .. 5.048 ms)
std dev              6.175 ms   (104.7 μs .. 13.21 ms)
variance introduced by outliers: 98% (severely inflated)

-}
