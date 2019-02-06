{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Guide.Logger.Methods (
  debugT, infoT, noticeT, warningT, errorT, alertT, criticalT, emergencyT,
  debugIO, infoIO, noticeIO, warningIO, errorIO, alertIO, criticalIO, emergencyIO,
  Di.push, Di.attr, value
  ) where

import Imports

import qualified Df1
import qualified Di.Core as DC
import Di.Monad
import qualified Di
import Guide.Logger.Types
import qualified Data.Text.Lazy as L


-- | Make a value for 'attr'.
value :: Show a => a -> Di.Value
value = Di.value . L.pack . show

-- | Error message handlers work in 'DiT' monad container.
debugT, infoT, noticeT, warningT, errorT, alertT, criticalT, emergencyT :: MonadDi Di.Level path Di.Message m => Text -> m ()
debugT     msg = Di.debug     . Df1.message $ toLText msg
infoT      msg = Di.info      . Df1.message $ toLText msg
noticeT    msg = Di.notice    . Df1.message $ toLText msg
warningT   msg = Di.warning   . Df1.message $ toLText msg
errorT     msg = Di.error     . Df1.message $ toLText msg
alertT     msg = Di.alert     . Df1.message $ toLText msg
criticalT  msg = Di.critical  . Df1.message $ toLText msg
emergencyT msg = Di.emergency . Df1.message $ toLText msg

-- | Error message handlers work in 'IO' monad container.
debugIO, infoIO, noticeIO, warningIO, errorIO, alertIO, criticalIO, emergencyIO :: DefDi -> Text -> IO ()
debugIO     di msg = DC.log di Df1.Debug     . Df1.message $ toLText msg
infoIO      di msg = DC.log di Df1.Info      . Df1.message $ toLText msg
noticeIO    di msg = DC.log di Df1.Notice    . Df1.message $ toLText msg
warningIO   di msg = DC.log di Df1.Warning   . Df1.message $ toLText msg
errorIO     di msg = DC.log di Df1.Error     . Df1.message $ toLText msg
alertIO     di msg = DC.log di Df1.Alert     . Df1.message $ toLText msg
criticalIO  di msg = DC.log di Df1.Critical  . Df1.message $ toLText msg
emergencyIO di msg = DC.log di Df1.Emergency . Df1.message $ toLText msg
