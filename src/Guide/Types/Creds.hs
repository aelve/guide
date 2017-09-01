{- |
Module      :  Guide.Types.Creds
Description :  Credentials for external providers (e.g.: GitHub authentication)
Copyright   :  (c) Aaron Friel
License     :  BSD-3

Maintainer  :  Aaron Friel <mayreply@aaronfriel.com>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

-}

module Guide.Types.Creds
(
  Creds,
    credsProvider,
    credsId,
    credsExtra,
  makeCreds,
  canAddCreds,
  PublicCreds,
  credsToPublic,
  publicCredsToCreds
)
where


import Imports

-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate
--
import qualified Data.Map as Map

data Creds = Creds {
  -- | The external provider of the credentials (e.g.: github.com)
  _credsProvider :: Text,
  -- | That provider's unique identifier for a user.
  _credsId       :: Text,
  -- | Additional fields that may be used, stored as a look-up table.
  _credsExtra    :: Map Text Text }
  deriving (Show)

deriveSafeCopySorted 0 'base ''Creds
makeLenses ''Creds

makeCreds :: Text -> Text -> Map Text Text -> Creds
makeCreds = Creds

-- | Determines if a 'Creds' is unique given a list of existing credentials.
--
-- See: 'credsDontOverlap'
canAddCreds :: Creds -> [Creds] -> Bool
canAddCreds creds listOfCreds =
  all (credsDontOverlap creds) (listOfCreds ^.. each)

-- | Determines if two credentials overlap, used in testing if credentials
-- can be added.
--
-- This is different from 'canCreateUser' because the providers *or*
-- the user IDs can differ.
credsDontOverlap :: Creds -> Creds -> Bool
credsDontOverlap credFoo credBar =
  any (\f -> f credFoo credBar) fieldTests
    where
      fieldNotEq field a b = a ^. field /= b ^. field
      fieldTests = [
          fieldNotEq credsProvider,
          fieldNotEq credsId ]

data PublicCreds = PublicCreds {
  publicCredsProvider :: Text,
  publicCredsId       :: Text }
  deriving (Show)

deriveSafeCopySorted 0 'base ''PublicCreds

credsToPublic :: Creds -> PublicCreds
credsToPublic Creds{..} =
  PublicCreds {
    publicCredsProvider = _credsProvider,
    publicCredsId       = _credsId
  }

publicCredsToCreds :: PublicCreds -> Creds
publicCredsToCreds PublicCreds{..} =
  Creds {
    _credsProvider = publicCredsProvider,
    _credsId       = publicCredsId,
    _credsExtra    = Map.empty
  }
