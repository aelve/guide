{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
A type for users. Currently unused.
-}
module Guide.Types.User
(
  User,
    userID,
    userName,
    userEmail,
    userPassword,
    userIsAdmin,
  makeUser,
  verifyUser,
  canCreateUser,
)
where

import Imports

-- acid-state
import Data.SafeCopy hiding (kind)
-- scrypt
import Crypto.Scrypt (Pass (..), EncryptedPass (..), encryptPassIO', getEncryptedPass, verifyPass')

import Guide.Utils
import Guide.SafeCopy

data User = User {
  -- | Unique, pseudorandom identifier for user.
  _userID          :: Uid User,
  -- | Unique username for user.
  _userName        :: Text,
  -- | Unique email address for user.
  _userEmail       :: Text,
  -- | Scrypt generated password field, contains salt + hash.
  _userPassword    :: Maybe ByteString,
  -- | Flag set if user is an administrator.
  _userIsAdmin     :: Bool
  }
  deriving (Show)

deriveSafeCopySorted 0 'base ''User
makeLenses ''User

-- | Creates a user object with an SCrypt encrypted password.
makeUser :: MonadIO m => Text -> Text -> ByteString -> m User
makeUser username email password = do
  encPass <- liftIO $ encryptPassIO' (Pass password)
  userid <- randomLongUid
  return User {
    _userID = userid,
    _userName = username,
    _userEmail = email,
    _userPassword = Just $ getEncryptedPass encPass,
    _userIsAdmin = False }

-- | Verifies a given password corresponds to a user's encrypted password.
verifyUser :: User -> ByteString -> Bool
verifyUser user password = 
  case user ^. userPassword of
    Just encPass -> verifyPass' (Pass password) (EncryptedPass encPass)
    Nothing -> False

-- | Looks at two users, and returns true if all unique fields are different.
canCreateUser :: User -> User -> Bool
canCreateUser userFoo userBar =
  all (\f -> f userFoo userBar) fieldTests
    where
      fieldNotEq field a b = a ^. field /= b ^. field
      fieldTests = [
        fieldNotEq userID,
        fieldNotEq userName,
        fieldNotEq userEmail ]
