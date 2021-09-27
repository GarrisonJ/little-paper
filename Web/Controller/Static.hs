module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Welcome
import Web.View.Users.FinishUserSetup

instance Controller StaticController where
    action WelcomeAction = do
        case currentUserOrNothing of
            Nothing -> render WelcomeView
            Just user -> do 
                -- If the user doesn't have a username we need to finsih setting them up
                if get #isSetup user
                    then redirectTo $ FollowedPostsAction Nothing
                    else redirectTo FinishUserSetupAction
