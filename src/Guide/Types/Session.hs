{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Guide.Types.Session
(
  GuideData (..),
  SpockSession,
  GuideSession,
    sess_id,
    sess_csrfToken,
    sess_validUntil,
    sess_data,
  unwrapSession,
  wrapSession,
  SessionId,
)
where

import Imports

-- Spock
import qualified Web.Spock as Spock
import Web.Spock.Internal.SessionManager (SessionId)
import qualified Web.Spock.Internal.SessionManager as Spock
-- Spock Session wrapper
import Data.Time.Clock ( UTCTime(..) )
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
-- acid-state
import qualified Data.Acid as Acid
import Data.SafeCopy hiding (kind)


type SpockSession conn st = Spock.Session conn GuideData st

data GuideData = GuideData ()
  deriving (Show, Eq)
deriveSafeCopy 0 'base ''GuideData

data GuideSession = GuideSession {
  _sess_id :: !SessionId,
  _sess_csrfToken :: !T.Text,
  _sess_validUntil :: !UTCTime,
  _sess_data :: !GuideData }
  deriving (Show, Eq)
deriveSafeCopy 0 'base ''GuideSession
makeLenses ''GuideSession

unwrapSession :: GuideSession -> SpockSession conn st
unwrapSession (GuideSession {..}) = Spock.Session {
    sess_id = _sess_id,
    sess_csrfToken = _sess_csrfToken,
    sess_validUntil = _sess_validUntil,
    sess_data = _sess_data
  }

wrapSession :: SpockSession conn st -> GuideSession
wrapSession (Spock.Session {..}) = GuideSession {
    _sess_id = sess_id,
    _sess_csrfToken = sess_csrfToken,
    _sess_validUntil = sess_validUntil,
    _sess_data = sess_data
  }
