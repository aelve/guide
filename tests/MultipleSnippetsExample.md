{{Multiple}}["Code1", "Code2", "Code3", "Code4"]
  {{Choice}}["Code4" : "-- | Compute factorial (1*2*...*n)"]
  factorial :: Int -> {{Choice}}["Code1" : "Int"; "Code2", "Code3", "Code4" : "Integer"]
  factorial n = {{Choice}}["Code1", "Code2" : "product";  "Code3", "Code4" : "foldl' 1 (*)"] [1..{{Choice}}["Code2", "Code3", "Code4" : "fromIntegral "]n]