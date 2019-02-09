module Guide.Logger.Types
(
  DefDiT,
  DefDi,
)
where

import Imports
import Di.Core
import Di.Monad
import Df1

-- | 'DefDiT' is ReaderT monad on DefDi.
type DefDiT = DiT Level Path Message IO

-- | 'DefDi' log messages of type msg,
-- with a particular importance level,
-- under a scope identified by path
type DefDi  = Di Level Path Message
