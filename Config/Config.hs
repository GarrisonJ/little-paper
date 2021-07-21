module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    option $ SMTP
        { host = "smtp.myisp.com"
        , port = 2525
        , credentials = Nothing -- or Just ("myusername","hunter2")
        }