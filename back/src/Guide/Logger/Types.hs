module Guide.Logger.Types where

import Imports
import Di.Core as Di
import Di.Monad as Di
import Df1
import qualified Di 

type DefDiT = DiT Level Text Message IO
type DefDi  = Di Level Text Message
