module Guide.Util.DOMUtil where

import Prelude
import Data.String (Pattern(..), Replacement(..), replaceAll, toLower)

-- | Clean up a string to get a valid key
mkKey :: String -> String
mkKey =
  toLower <<< replaceAll (Pattern " ") (Replacement "-")
