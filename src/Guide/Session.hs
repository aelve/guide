{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Guide.Session
(
  newAcidSessionStore
)
where

import Imports hiding (toList)

-- Spock
import Web.Spock.Config
-- acid-state
import Data.Acid as Acid

import Guide.State
import Guide.Types.Session

-- |Queries for all user sessions and then removes sessions unless predicate matches.
filterSessions :: AcidState GlobalState -> (SpockSession conn st -> Bool) -> IO ()
filterSessions db f = do
  sessions <- Acid.query db GetSessions
  forM_ sessions $ \sess -> do
    unless (f $ unwrapSession sess) $
      Acid.update db $ DeleteSession (sess ^. sess_id)

-- |Queries for all user sessions and then performs an operation over all.
mapSessions :: MonadIO m => AcidState GlobalState -> (SpockSession conn st -> m (SpockSession conn st)) -> m ()
mapSessions db f = do
  sessions <- liftIO $ Acid.query db GetSessions
  forM_ sessions $ \sess -> do
    newSess <- f (unwrapSession sess)
    liftIO $ Acid.update db $ StoreSession (wrapSession newSess)

-- |Wraps an STM session store and periodically commits session to the database.
newAcidSessionStore' :: AcidState GlobalState -> IO (SessionStore (SpockSession conn st) IO)
newAcidSessionStore' db = do
  -- See Note [Session Linearizability]
  lock <- newMVar ()
  return $ SessionStore {
    ss_runTx = withMVar lock . const,
    ss_loadSession = \sessId -> do
      sess <- Acid.query db $ LoadSession sessId
      return $ unwrapSession <$> sess,
    ss_deleteSession = Acid.update db . DeleteSession,
    ss_storeSession = Acid.update db . StoreSession . wrapSession,
    ss_toList = do
      sessions <- Acid.query db GetSessions
      return $ map unwrapSession sessions,
    ss_filterSessions = filterSessions db,
    ss_mapSessions = mapSessions db
    }

newAcidSessionStore :: AcidState GlobalState -> IO (SessionStoreInstance (SpockSession conn st))
newAcidSessionStore db = SessionStoreInstance <$> newAcidSessionStore' db

{- Note [Session Linearizability]

Acid-State transactions are, I believe, serializable by default.
Updates can be issued in parallel, and the temporal ordering of each update
can vary, but each atomic update can be executed in arbitrary order.

Acid-state may also be sequentially consistent, not sure. It's definitely
not linearizable, which is a property we really want for session data
types. In other words, we can have data races.

Consider two actions taken by an administrator:

* Administrator updates user profiles to remove access rights,
  running GetSession and then StoreSession, via filterSessions or mapSessions.

* Eve at the same time updates their user profile to change their user name,
  running LoadSession and then StoreSession.

Since filterSession is not atomic, this sequence could occur:

|   Process   |     Command   | Context
|    Admin    |  GetSessions  | mapSessions runs GetSessions, obtaining a list of all sessions
|     Eve     |  LoadSession  | user profile page view
|    Admin    |  StoreSession | mapSessions runs StoreSession for Eve, removing permissions
|     Eve     |  StoreSession | Eve clicks "save profile" which refreshes her session

This is a classic race condition. So we use a lock on the Session Store.

-}