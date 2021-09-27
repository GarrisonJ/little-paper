module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail
import System.Environment
import IHP.OAuth.Google.Config ( initGoogleOAuth )

import qualified IHP.Log as Log
import IHP.Log.Types
    ( LogLevel(Debug),
      newLogger,
      withTimeFormatter,
      LoggerSettings(level, formatter) )


config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")


    logger <- liftIO $ newLogger def {
      level = Debug,
      formatter = withTimeFormatter
    }
    option logger

    initGoogleOAuth
    -- other options here, then add:

  {-
    awsSesAccessKey <- liftIO $ fromString <$> System.Environment.getEnv "IHP_AWS_SES_ACCESS_KEY"
    awsSesSecretKey <- liftIO $ fromString <$> System.Environment.getEnv "IHP_AWS_SES_SECRET_KEY"
    option $ SES
        { accessKey = awsSesAccessKey
        , secretKey = awsSesSecretKey
        , region = "us-west-2" -- YOUR REGION
        }
  -}
