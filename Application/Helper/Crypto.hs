module Application.Helper.Crypto where

import IHP.ControllerPrelude
import qualified Crypto.PasswordStore
import IHP.AuthSupport.Authentication

tokenStrength :: Int
tokenStrength = 17

verifyToken :: Text -> Text -> Bool
verifyToken tokenHash plainText = Crypto.PasswordStore.verifyPassword (cs plainText) (cs tokenHash)

hashToken :: Text -> IO Text
hashToken plainText = cs <$> Crypto.PasswordStore.makePassword (cs plainText) tokenStrength

randomText :: IO Text
randomText = generateAuthenticationToken