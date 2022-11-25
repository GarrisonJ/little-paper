module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Welcome
import Web.View.Static.Terms
import Web.View.Static.PrivacyPolicy
import Web.View.Users.FinishUserSetup

instance Controller StaticController where
    action TermsAction = render TermsView
    action PrivacyPolicyAction = render PrivacyPolicyView

    action WelcomeAction = do
        case currentUserOrNothing of
            Nothing -> do
                lockUserCreation <- lockUserCreation_
                render WelcomeView { .. }

            Just user -> do 
                -- If the user doesn't have a username we need to finsih setting them up
                if get #isSetup user
                    then redirectTo $ FollowedPostsAction Nothing
                    else redirectTo FinishUserSetupAction

lockUserCreation_ :: (?modelContext::ModelContext) => IO Bool
lockUserCreation_ = do
      loginState <- query @LoginState
                  |> fetchOneOrNothing

      case loginState of
            Nothing -> pure False
            Just loginState -> do
                  -- Count number of users
                  numberOfUsers <- query @User
                                    |> fetchCount
                  let maxUsers = get #maxUsers loginState
                  let isLocked = get #isUserCreationLocked loginState
                  pure $ numberOfUsers >= maxUsers || isLocked