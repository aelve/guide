module Data.Time.NominalDiffTime
  ( NominalDiffTime (..)
  ) where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds)

newtype NominalDiffTime = NominalDiffTime Seconds

derive instance ntNominalDiffTime :: Newtype NominalDiffTime _
derive instance gNominalDiffTime :: Generic NominalDiffTime
derive instance eqNominalDiffTime :: Eq NominalDiffTime
derive instance ordNominalDiffTime :: Ord NominalDiffTime
instance showNominalDiffTime :: Show NominalDiffTime where
 show = gShow
