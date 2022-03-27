module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User
    action CreateSessionAction = Sessions.createSessionAction @User
    action DeleteSessionAction = do
                        unless (isNothing currentUserOrNothing) $ logout currentUser
                        redirectTo NewSessionAction

instance Sessions.SessionsControllerConfig User where
    beforeLogin user = do
        unless (get #isConfirmed user) do
            setErrorMessage "Please click the confirmation link we sent to your email"
            redirectTo NewSessionAction
