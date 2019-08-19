-- | Deprecated as of 2019-07-25.
--
-- TODO: remove after we have migrated from acid-state.
--
-- Items could have different colors; this module provides type 'Hue' and
-- used to provide a palette for turning hues into actual colors.
module Guide.Types.Hue
(
  Hue(..),
)
where


import Imports

import Data.SafeCopy hiding (kind)

import qualified Data.Aeson as Aeson


data Hue = NoHue | Hue Int
  deriving (Eq, Ord, Generic, Data)

deriveSafeCopySimple 1 'extension ''Hue

instance Aeson.ToJSON Hue where
  toJSON NoHue   = Aeson.toJSON (0 :: Int)
  toJSON (Hue n) = Aeson.toJSON n

data Hue_v0 = NoHue_v0 | Hue_v0 Int

deriveSafeCopy 0 'base ''Hue_v0

instance Migrate Hue where
  type MigrateFrom Hue = Hue_v0
  migrate NoHue_v0   = NoHue
  migrate (Hue_v0 i) = Hue i

instance Show Hue where
  show NoHue   = "0"
  show (Hue n) = show n
