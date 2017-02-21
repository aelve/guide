{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
A type for users. Currently unused.
-}
module Guide.Types.User
(
  User(..),
    userID,
    userName,
    userEmail,
    userPassword,
    userIsAdmin,
  makeUser,
  canCreateUser,
)
where

import Imports

-- acid-state
import Data.SafeCopy hiding (kind)
-- scrypt
import Crypto.Scrypt (Pass, encryptPassIO', getEncryptedPass)

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

makeUser :: MonadIO m => Text -> Text -> Pass -> m User
makeUser username email password = do
  encPass <- liftIO $ encryptPassIO' password
  userid <- randomLongUid
  return User {
    _userID = userid,
    _userName = username,
    _userEmail = email,
    _userPassword = Just $ getEncryptedPass encPass,
    _userIsAdmin = False }

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
