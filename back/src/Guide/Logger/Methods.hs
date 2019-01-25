{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Guide.Logger.Methods (
  debugT, infoT, noticeT, warningT, errorT, alertT, criticalT, emergencyT,
  debugIO, infoIO, noticeIO, warningIO, errorIO, alertIO, criticalIO, emergencyIO,
  Di.Monad.push,
  ) where

import Imports

import qualified Df1
import qualified Di.Core as DC
import Di.Monad
import Di
import Guide.Logger.Types

debugT, infoT, noticeT, warningT, errorT, alertT, criticalT, emergencyT :: MonadDi Level path Message m => Text -> m ()
debugT     msg = Di.debug     . Df1.message $ fromStrict msg
infoT      msg = Di.info      . Df1.message $ fromStrict msg
noticeT    msg = Di.notice    . Df1.message $ fromStrict msg
warningT   msg = Di.warning   . Df1.message $ fromStrict msg
errorT     msg = Di.error     . Df1.message $ fromStrict msg
alertT     msg = Di.alert     . Df1.message $ fromStrict msg
criticalT  msg = Di.critical  . Df1.message $ fromStrict msg
emergencyT msg = Di.emergency . Df1.message $ fromStrict msg

debugIO, infoIO, noticeIO, warningIO, errorIO, alertIO, criticalIO, emergencyIO :: DefDi -> Text -> IO ()
debugIO     di msg = DC.log di Df1.Debug     . Df1.message $ fromStrict msg
infoIO      di msg = DC.log di Df1.Info      . Df1.message $ fromStrict msg
noticeIO    di msg = DC.log di Df1.Notice    . Df1.message $ fromStrict msg
warningIO   di msg = DC.log di Df1.Warning   . Df1.message $ fromStrict msg
errorIO     di msg = DC.log di Df1.Error     . Df1.message $ fromStrict msg
alertIO     di msg = DC.log di Df1.Alert     . Df1.message $ fromStrict msg
criticalIO  di msg = DC.log di Df1.Critical  . Df1.message $ fromStrict msg
emergencyIO di msg = DC.log di Df1.Emergency . Df1.message $ fromStrict msg
