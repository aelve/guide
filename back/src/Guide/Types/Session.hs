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

import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate
import Web.Spock.Internal.SessionManager (SessionId)

import Guide.Types.User
import Guide.Uid

import qualified Web.Spock.Internal.SessionManager as Spock


type SpockSession conn st = Spock.Session conn GuideData st

-- | GuideData is the session data exposed by Spock.SessionAction operations.
data GuideData = GuideData {
    -- | If logged in, must be a valid userID
    _sessionUserID :: Maybe (Uid User)
  }
  deriving (Show, Eq, Data)

deriveSafeCopySorted 0 'base ''GuideData
makeLenses ''GuideData

emptyGuideData :: GuideData
emptyGuideData = GuideData {
  _sessionUserID = Nothing }

data GuideSession = GuideSession {
  _sess_id         :: !SessionId,
  _sess_csrfToken  :: !Text,
  _sess_validUntil :: !UTCTime,
  _sess_data       :: !GuideData }
  deriving (Show, Eq, Data)

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
wrapSession s = GuideSession {
    _sess_id = Spock.sess_id s,
    _sess_csrfToken = Spock.sess_csrfToken s,
    _sess_validUntil = Spock.sess_validUntil s,
    _sess_data = Spock.sess_data s
  }
