{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Guide.Logger.Methods
(
  -- * Loggers in 'MonadDi'
  debugT, infoT, noticeT, warningT, errorT, alertT, criticalT, emergencyT,

  -- * Loggers in 'IO'
  debugIO, infoIO, noticeIO, warningIO, errorIO, alertIO, criticalIO, emergencyIO,

  -- * Utils
  push, attr, value,
  ) where

import Imports

import qualified Df1
import qualified Di.Core as DC
import qualified Di
import Guide.Logger.Types

debugT, infoT, noticeT, warningT, errorT, alertT, criticalT, emergencyT
  :: Di.MonadDf1 m => Text -> m ()
debugT     msg = Di.debug     . Df1.message $ toLText msg
infoT      msg = Di.info      . Df1.message $ toLText msg
noticeT    msg = Di.notice    . Df1.message $ toLText msg
warningT   msg = Di.warning   . Df1.message $ toLText msg
errorT     msg = Di.error     . Df1.message $ toLText msg
alertT     msg = Di.alert     . Df1.message $ toLText msg
criticalT  msg = Di.critical  . Df1.message $ toLText msg
emergencyT msg = Di.emergency . Df1.message $ toLText msg

debugIO, infoIO, noticeIO, warningIO, errorIO, alertIO, criticalIO, emergencyIO
  :: DefDi -> Text -> IO ()
debugIO     di msg = DC.log di Df1.Debug     . Df1.message $ toLText msg
infoIO      di msg = DC.log di Df1.Info      . Df1.message $ toLText msg
noticeIO    di msg = DC.log di Df1.Notice    . Df1.message $ toLText msg
warningIO   di msg = DC.log di Df1.Warning   . Df1.message $ toLText msg
errorIO     di msg = DC.log di Df1.Error     . Df1.message $ toLText msg
alertIO     di msg = DC.log di Df1.Alert     . Df1.message $ toLText msg
criticalIO  di msg = DC.log di Df1.Critical  . Df1.message $ toLText msg
emergencyIO di msg = DC.log di Df1.Emergency . Df1.message $ toLText msg

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
