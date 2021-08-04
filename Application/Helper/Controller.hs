module Application.Helper.Controller (verifyToken, hashToken, randomText) where

import IHP.ControllerPrelude

-- Here you can add functions which are available in all your controllers
import Application.Helper.Crypto (verifyToken, hashToken, randomText)