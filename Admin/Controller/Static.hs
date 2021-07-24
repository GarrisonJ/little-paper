module Admin.Controller.Static where
import Admin.Controller.Prelude
import Admin.View.Static.Welcome

instance Controller StaticController where
    action WelcomeAction = do
        case currentAdminOrNothing @Admin of
            Nothing -> render WelcomeView
            Just _ -> redirectTo UsersAdminAction
