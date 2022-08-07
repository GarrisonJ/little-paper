module Application.Helper.Controller (
      module Application.Helper.Crypto,
      getUserDay,
      isUsernameChars,
      usernameMaxLength,
      usernameMinLength,
      rightToMaybe
) where

import IHP.ControllerPrelude
import Application.Helper.Crypto
import Data.Time.Zones.All
import Data.Time
import Data.Time.Zones (utcToLocalTimeTZ, utcToLocalTimeTZ ,utcTZ)
import Data.Text.Encoding (encodeUtf8)
import Text.Regex.TDFA ( (=~) )

getUserDay :: Text -> IO Day
getUserDay preferedTimezone = do
      let preferedTimezoneParsed = fromMaybe utcTZ $ tzByName $ encodeUtf8 preferedTimezone
      currentLocalTime <- utcToLocalTimeTZ preferedTimezoneParsed <$> getCurrentTime
      return $ localDay currentLocalTime

isUsernameChars :: Text -> ValidatorResult
isUsernameChars text | text =~ ("([A-Za-z0-9\\_]+)" :: Text) = Success
isUsernameChars text = Failure "Your username can only contain letters, numbers and '_'"

usernameMaxLength :: Int
usernameMaxLength = 15
usernameMinLength :: Int
usernameMinLength = 3

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x