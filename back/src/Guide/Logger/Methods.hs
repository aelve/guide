{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Guide.Logger.Methods
(
  -- * Loggers in 'MonadDi'
  debugT, infoT, warningT, errorT,

  -- * Loggers in 'IO'
  debugIO, infoIO, warningIO, errorIO,

  -- * Utils
  push, attr, value,
)
where

import Imports

import qualified Df1
import qualified Di.Core as DC
import qualified Di
import Guide.Logger.Types

debugT, infoT, warningT, errorT :: Di.MonadDf1 m => Text -> m ()
debugT     msg = Di.debug     . Df1.message $ toLText msg
infoT      msg = Di.info      . Df1.message $ toLText msg
warningT   msg = Di.warning   . Df1.message $ toLText msg
errorT     msg = Di.error     . Df1.message $ toLText msg

debugIO, infoIO, warningIO, errorIO :: DefDi -> Text -> IO ()
debugIO     di msg = DC.log di Df1.Debug     . Df1.message $ toLText msg
infoIO      di msg = DC.log di Df1.Info      . Df1.message $ toLText msg
warningIO   di msg = DC.log di Df1.Warning   . Df1.message $ toLText msg
errorIO     di msg = DC.log di Df1.Error     . Df1.message $ toLText msg

-- | Push method name to the log path.
push :: Di.MonadDf1 m => Text -> m a -> m a
push key = Di.push (Di.segment key)

-- | Push an attribute to the log path.
--
-- Like 'Di.attr', but automatically 'show's the value you want to push.
attr :: (Di.MonadDf1 m, Show val) => Text -> val -> m a -> m a
attr key = Di.attr (Di.key key) . value

-- | Like 'Di.value', but uses 'show' instead of 'Di.ToValue'.
--
-- We don't want to write all the 'Di.ToValue' instances for all types that we use.
value :: Show a => a -> Di.Value
value = Di.value . toLText . show
