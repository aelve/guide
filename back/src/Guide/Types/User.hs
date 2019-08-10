{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | A type for users. Currently unused.
module Guide.Types.User
(
  User(..),
  UserLenses(..),
  makeUser,
  verifyUser,
  canCreateUser,
  PublicUser(..),
  userToPublic,
  publicUserToUser
)
where


import Imports

-- acid-state
import Data.SafeCopy hiding (kind)
import Data.SafeCopy.Migrate
-- scrypt
import Crypto.Scrypt (EncryptedPass (..), Pass (..), encryptPassIO', getEncryptedPass, verifyPass')

import Guide.Utils


data User = User {
  -- | Unique, pseudorandom identifier for user.
  userID       :: Uid User,
  -- | Unique username for user.
  userName     :: Text,
  -- | Unique email address for user.
  userEmail    :: Text,
  -- | Scrypt generated password field, contains salt + hash.
  userPassword :: Maybe ByteString,
  -- | Flag set if user is an administrator.
  userIsAdmin  :: Bool
  }
  deriving (Show)

deriveSafeCopySorted 0 'base ''User
makeClassWithLenses ''User

-- | Creates a user object with an SCrypt encrypted password.
makeUser :: MonadIO m => Text -> Text -> ByteString -> m User
makeUser username email password = do
  encPass <- liftIO $ encryptPassIO' (Pass password)
  userid <- randomLongUid
  return User {
    userID = userid,
    userName = username,
    userEmail = email,
    userPassword = Just $ getEncryptedPass encPass,
    userIsAdmin = False }

-- | Verifies a given password corresponds to a user's encrypted password.
verifyUser :: User -> ByteString -> Bool
verifyUser user password =
  case userPassword user of
    Just encPass -> verifyPass' (Pass password) (EncryptedPass encPass)
    Nothing      -> False

-- | Looks at two users, and returns true if all unique fields are different.
canCreateUser :: User -> User -> Bool
canCreateUser $(fieldsPrefixed "a_" 'User) $(fieldsPrefixed "b_" 'User) =
  a_userID /= b_userID &&
  a_userName /= b_userName &&
  a_userEmail /= b_userEmail
  where
    -- Ignored fields
    _ = (a_userIsAdmin, b_userIsAdmin)
    _ = (a_userPassword, b_userPassword)

-- | 'PublicUser' contains all safe User data.
-- Removed from 'User':
-- * Password
data PublicUser = PublicUser {
  publicUserID      :: Uid User,
  publicUserName    :: Text,
  publicUserEmail   :: Text,
  publicUserIsAdmin :: Bool}
  deriving (Show)

deriveSafeCopySorted 0 'base ''PublicUser

-- | Converts 'User' to 'PublicUser' type.
userToPublic :: User -> PublicUser
userToPublic $(fields 'User) =
  PublicUser {
    publicUserID      = userID,
    publicUserName    = userName,
    publicUserEmail   = userEmail,
    publicUserIsAdmin = userIsAdmin
  }
  where
    -- Ignored fields
    _ = userPassword

-- | Converts 'PublicUser' to 'User' filling password with Nothing.
publicUserToUser :: PublicUser -> User
publicUserToUser $(fields 'PublicUser) =
  User {
    userID       = publicUserID,
    userName     = publicUserName,
    userEmail    = publicUserEmail,
    userPassword = Nothing,
    userIsAdmin  = publicUserIsAdmin
  }
