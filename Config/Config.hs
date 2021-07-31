module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail
import System.Environment

config :: ConfigBuilder
config = do
    -- other options here, then add:

    awsSesAccessKey <- liftIO $ fromString <$> System.Environment.getEnv "IHP_AWS_SES_ACCESS_KEY"
    awsSesSecretKey <- liftIO $ fromString <$> System.Environment.getEnv "IHP_AWS_SES_SECRET_KEY"
    option $ SES
        { accessKey = awsSesAccessKey
        , secretKey = awsSesSecretKey
        , region = "us-west-2" -- YOUR REGION
        }
