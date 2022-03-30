{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Controller.GoogleOAuth where

import Web.Controller.Prelude
import Web.Controller.Sessions ()
import IHP.OAuth.Google.Controller
    ( GoogleOAuthControllerConfig(beforeCreateUser, createUser),
      googleConnectCallbackAction,
      newSessionWithGoogleAction )
import qualified IHP.OAuth.Google.Types as Google
import Web.Controller.Static
import Control.Monad.Except
import IHP.MailPrelude (createRecord)
import Admin.Controller.Prelude
import Data.Coerce

instance Controller Google.GoogleOAuthController where
    action Google.NewSessionWithGoogleAction = newSessionWithGoogleAction @User
    action Google.GoogleConnectCallbackAction = googleConnectCallbackAction @User

newtype UserHack = UserHack User deriving (Eq,Show,CanCreate)
instance GoogleOAuthControllerConfig UserHack

defaultUserCreateMethod :: (?context::ControllerContext, ?modelContext::ModelContext) => User -> Google.GoogleClaims -> IO User
defaultUserCreateMethod user googleClaims = coerce <$> (createUser @UserHack) (UserHack user) googleClaims

instance GoogleOAuthControllerConfig User where
    createUser user googleClaims = do
        userCreationIsLocked <- lockUserCreation_
        when userCreationIsLocked $ do
            setErrorMessage "New users are currently locked. Please try again later."
            redirectBack
        defaultUserCreateMethod user googleClaims

    beforeCreateUser user googleClaims = do
        user |> set #isConfirmed True -- This will auto-confirm your user's email