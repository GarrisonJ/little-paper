module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Welcome

instance Controller StaticController where
    action WelcomeAction = do
        case currentUserOrNothing of
            Nothing -> render WelcomeView
            Just _ -> redirectTo FollowedPostsAction
