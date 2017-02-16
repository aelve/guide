{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
A type for users. Currently unused.
-}
module Guide.Types.User
(
  User(..),
  makeUser,
)
where

import Imports

-- acid-state
import Data.SafeCopy hiding (kind)
-- scrypt
import Crypto.Scrypt (Pass, encryptPassIO', getEncryptedPass)

import Guide.Utils
import Guide.SafeCopy
-- import Guide.Types.Core
-- import Guide.Types.Edit

data User = User {
  userID          :: Uid User,
  userName        :: Text,
  userEmail       :: Text,
  userPassword    :: Maybe ByteString
  }
  deriving (Show)

makeUser :: MonadIO m => Text -> Text -> Pass -> m User
makeUser username email password = do
  encPass <- liftIO $ encryptPassIO' password
  userID <- randomLongUid
  return User {
    userID = userID,
    userName = username,
    userEmail = email,
    userPassword = Just $ getEncryptedPass encPass
  }

deriveSafeCopySorted 0 'base ''User
