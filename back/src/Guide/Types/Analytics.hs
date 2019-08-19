{-# LANGUAGE TemplateHaskell #-}


-- | Types for analytics.
--
--  * We collect information about 'Action's that users perform. An action
--    can be a page visit, a search, or an edit.
--
--  * Some actions have referrers. For instance, when a user goes from
--    Reddit to our site, Reddit is the referrer.
--
--  * We also collect additional information about users performing actions,
--    such as date and time when the action was performed.
module Guide.Types.Analytics
(
  Action(..),            -- TODO: this is only needed for a 'GlobalState'
                         -- migration, and should be removed after the
                         -- migration is done.
  Referrer(..),
  ActionDetails(..),     -- TODO: this is only needed for a 'GlobalState'
                         -- migration, and should be removed after the
                         -- migration is done.
)
where


import Imports

-- Network
import Data.IP
-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate

import Guide.Types.Core
import Guide.Types.Edit
import Guide.Utils


data Action
  = ActionMainPageVisit
  | ActionCategoryVisit (Uid Category)
  | ActionSearch Text
  | ActionEdit Edit
  deriving (Show)

deriveSafeCopySimple 0 'base ''Action

data Referrer = InternalReferrer Url | ExternalReferrer Url
  deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''Referrer

data ActionDetails = ActionDetails {
  actionIP        :: Maybe IP,
  actionDate      :: UTCTime,
  actionReferrer  :: Maybe Referrer,
  actionUserAgent :: Maybe Text }
  deriving (Show)

deriveSafeCopySorted 3 'extension ''ActionDetails

changelog ''ActionDetails (Current 3, Past 2) []
deriveSafeCopySorted 2 'base ''ActionDetails_v2
