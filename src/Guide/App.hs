{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

{- |
App module defines types used by the Spock framework.

-}
module Guide.App 
where

-- hvect
import Data.HVect
-- Spock
import Web.Spock

import Guide.Types.User (User)
import Guide.Types.Session (GuideData)
import Guide.ServerStuff (ServerState)

-- | Type of connection, currently unused. (Acid-State DB stored in 'ServerState')
type GuideConn        = ()
-- | Type of user session payload.
type GuideSessionData = GuideData
-- | Type of server state, accessed with 'getState'.
type GuideState       = ServerState

-- | The fully qualified type of a Spock application/route.
type GuideM ctx r = SpockCtxM ctx GuideConn GuideData ServerState r

-- | Type of a root application.
type GuideApp ctx = GuideM ctx ()

-- | Type of a Guide action with a generic context.
type GuideAction ctx r = ActionCtxT ctx (WebStateM GuideConn GuideData ServerState) r

data IsAdmin = IsAdmin

type AuthM ctx r = forall n xs. (ctx ~ HVect xs, ListContains n User xs) => GuideM ctx r
type AuthAction ctx r = forall n xs. (ctx ~ HVect xs, ListContains n User xs) => GuideAction ctx r

type AdminM ctx r = forall n xs. (ctx ~ HVect xs, ListContains n IsAdmin xs) => GuideM ctx r
type AdminAction ctx r = forall n xs. (ctx ~ HVect xs, ListContains n IsAdmin xs) => GuideAction ctx r
