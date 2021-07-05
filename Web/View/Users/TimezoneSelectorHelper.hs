module Web.View.Users.TimezoneSelectorHelper where

import Web.View.Prelude
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Zones.All (TZLabel, toTZName, fromTZName)

type TimezoneText = Text
instance CanSelect TimezoneText where
    type SelectValue TimezoneText = TimezoneText
    selectLabel x = x
    selectValue x = x

allTimezones :: [TimezoneText]
allTimezones = map decodeUtf8 $ map toTZName $ enumFrom minBound