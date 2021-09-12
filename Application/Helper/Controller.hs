module Application.Helper.Controller (
      module Application.Helper.Crypto,
      getUserDay
) where

import IHP.ControllerPrelude
import Application.Helper.Crypto
import Data.Time.Zones.All
import Data.Time
import Data.Time.Zones (utcToLocalTimeTZ, utcToLocalTimeTZ ,utcTZ)
import Data.Text.Encoding (encodeUtf8)


getUserDay :: Text -> IO (Day)
getUserDay preferedTimezone = do
      let preferedTimezoneParsed = fromMaybe utcTZ $ tzByName $ encodeUtf8 preferedTimezone
      currentLocalTime <- (utcToLocalTimeTZ preferedTimezoneParsed) <$> getCurrentTime
      return $ localDay currentLocalTime