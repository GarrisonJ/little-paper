module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail
import System.Environment
import IHP.OAuth.Google.Config ( initGoogleOAuth )
import IHP.Sentry
import IHP.FileStorage.Config


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

    initSentry "https://6d6350e9bbb949cc93e1c4c12589db87@o1014737.ingest.sentry.io/5979975"

    logger <- liftIO $ newLogger def {
      level = Debug,
      formatter = withTimeFormatter
    }
    option logger

    initGoogleOAuth

    initS3Storage "us-west-2" "little-paper-dev"

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
