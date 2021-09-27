module Web.Controller.GoogleOAuth where

import Web.Controller.Prelude
    ( Bool(True),
      User,
      User'(email, googleUserId, id, lockedAt),
      set,
      (|>),
      Controller(action), undefined )
import Web.Controller.Sessions ()
import IHP.OAuth.Google.Controller
    ( GoogleOAuthControllerConfig(beforeCreateUser, createUser),
      googleConnectCallbackAction,
      newSessionWithGoogleAction )
import qualified IHP.OAuth.Google.Types as Google

instance Controller Google.GoogleOAuthController where
    action Google.NewSessionWithGoogleAction = newSessionWithGoogleAction @User
    action Google.GoogleConnectCallbackAction = googleConnectCallbackAction @User


instance GoogleOAuthControllerConfig User where
    beforeCreateUser user googleClaims = do 
        user |> set #isConfirmed True -- This will auto-confirm your user's email