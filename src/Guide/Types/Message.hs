{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Guide.Types.Message
(
  Message(..),
)
where

import Imports

-- Text
import qualified Data.Text.All as T
-- acid-state
import Data.SafeCopy hiding (kind)

import Guide.Utils
import Guide.SafeCopy
import Guide.Types.Core
import Guide.Types.Edit

data Message = Message {
  messageID       :: Uid Message,
  messageDate     :: UTCTime,
  messageText     :: Text }
  deriving (Show)

deriveSafeCopySorted 0 'base ''Message
