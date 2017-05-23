{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Guide.Types.Session
(
  GuideData (..),
    sessionUserID,
  emptyGuideData,
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
import Web.Spock.Internal.SessionManager (SessionId)
import qualified Web.Spock.Internal.SessionManager as Spock
-- Spock Session wrapper
import Data.Time.Clock ( UTCTime(..) )
import qualified Data.Text as T
-- acid-state
import Data.SafeCopy hiding (kind)

import Guide.SafeCopy
import Guide.Utils
import Guide.Types.User

type SpockSession conn st = Spock.Session conn GuideData st

-- | GuideData is the session data exposed by Spock.SessionAction operations.
data GuideData = GuideData {
    -- | If logged in, must be a valid userID
    _sessionUserID :: Maybe (Uid User)
  }
  deriving (Show, Eq)

deriveSafeCopySorted 0 'base ''GuideData
makeLenses ''GuideData

emptyGuideData :: GuideData
emptyGuideData = GuideData {
  _sessionUserID = Nothing }

data GuideSession = GuideSession {
  _sess_id :: !SessionId,
  _sess_csrfToken :: !T.Text,
  _sess_validUntil :: !UTCTime,
  _sess_data :: !GuideData }
  deriving (Show, Eq)

deriveSafeCopySorted 0 'base ''GuideSession
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
