{-# LANGUAGE NoImplicitPrelude #-}


{- |
Imports used in the whole codebase. (All modules import this one instead of
the "Prelude".)
-}
module Imports
(
  module X,
  LByteString,
)
where


import BasePrelude as X hiding (Category, GeneralCategory, lazy, (&))
-- Lists
import Data.List.Index as X
-- Lenses
import Lens.Micro.Platform as X
-- Monads and monad transformers
import Control.Monad.IO.Class as X
import Control.Monad.State as X
import Control.Monad.Reader as X
-- Common types
import Data.Text.All as X (Text, LText)
import Data.ByteString as X (ByteString)
import Data.Map as X (Map)
import Data.Set as X (Set)
-- Time
import Data.Time as X
-- Files
import System.Directory as X
import System.FilePath as X
-- Deepseq
import Control.DeepSeq as X
-- Hashable
import Data.Hashable as X
-- Lazy bytestring
import qualified Data.ByteString.Lazy as BSL


type LByteString = BSL.ByteString
-- LText is already provided by Data.Text.All
