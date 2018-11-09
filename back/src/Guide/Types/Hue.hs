{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}


{- |
Items can have different colors; this module provides type 'Hue' and a
palette for turning hues into actual colors.
-}
module Guide.Types.Hue
(
  Hue(..),
  hueToDarkColor,
  hueToLightColor,
)
where


import Imports

import Data.SafeCopy hiding (kind)

import qualified Data.Aeson as A


data Hue = NoHue | Hue Int
  deriving (Eq, Ord, Generic, Data)

deriveSafeCopySimple 1 'extension ''Hue

instance A.ToJSON Hue where
  toJSON NoHue   = A.toJSON (0 :: Int)
  toJSON (Hue n) = A.toJSON n

data Hue_v0 = NoHue_v0 | Hue_v0 Int

deriveSafeCopy 0 'base ''Hue_v0

instance Migrate Hue where
  type MigrateFrom Hue = Hue_v0
  migrate NoHue_v0   = NoHue
  migrate (Hue_v0 i) = Hue i

instance Show Hue where
  show NoHue   = "0"
  show (Hue n) = show n

-- Colors taken from:
-- <https://www.google.com/design/spec/style/color.html#color-color-palette>
hueToDarkColor :: Hue -> Text
hueToDarkColor NoHue = "#D6D6D6"  -- the color for gray isn't from Google's
                                  -- palette, since their “100” is too light
hueToDarkColor (Hue i) = table !! ((i-1) `mod` length table)
  where
    -- the “100” colors
    table = ["#D1C4E9",   -- deep purple
             "#C8E6C9",   -- green
             "#FFECB3",   -- amber
             "#BBDEFB",   -- blue
             "#FFCDD2",   -- red
             "#D7CCC8",   -- brown
             "#B2DFDB",   -- teal
             "#F0F4C3"]   -- lime

hueToLightColor :: Hue -> Text
hueToLightColor NoHue = "#F0F0F0"  -- the color for gray isn't from Google's
                                   -- palette, since their “50” is too light
hueToLightColor (Hue i) = table !! ((i-1) `mod` length table)
  where
    -- the “50” colors
    table = ["#EDE7F6",   -- deep purple
             "#E8F5E9",   -- green
             "#FFF8E1",   -- amber
             "#E3F2FD",   -- blue
             "#FFEBEE",   -- red
             "#EFEBE9",   -- brown
             "#E0F2F1",   -- teal
             "#F9FBE7"]   -- lime
