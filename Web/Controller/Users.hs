module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Index
import Web.View.Users.New
import Web.View.Users.FinishUserSetup
import Web.View.Users.Edit
import Web.View.Users.Show
import Web.Controller.Static
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)
import Text.Regex.TDFA ( (=~) )
import Web.Mail.Users.Confirmation
import System.Random
import qualified IHP.Log as Log


instance Controller UsersController where

    -- List all of the users.
    action UsersAction = do
        ensureIsUser

        (userQuery, pagination) <- query @User
            |> filterWhere (#isConfirmed, True)
            |> filterWhere (#isSetup, True)
            |> filterWhereNot (#id, currentUserId)
            |> paginateWithOptions
                (defaultPaginationOptions
                    |> set #maxItems 10)

        users <- userQuery |> fetch

        follows' <- query @UserFollow
            |> filterWhere (#followerId, currentUserId)
            |> filterWhereIn (#followedId, ids users)
            |> fetch

        let follows = map (get #followedId) follows'

        render IndexView { .. }

    -- The new user form
    -- This should have no restrictions
    action NewUserAction = do
        -- Don't let loggedin users create new users
        case currentUserOrNothing of
            Just _ -> redirectToPath "/"
            Nothing -> do
                let user = newRecord
                render NewView { .. }

    -- If a user signs up with Google OAuth, we still need to finish setting them up.
    -- Also, if they already have an account with that email, we need to see if we can 
    -- use that account instead of creating a new one. 
    -- It's important to check if the user has confirm their email before allow them to use the
    -- account.
    action FinishUserSetupAction = do
        case currentUserOrNothing of
            Nothing -> redirectToPath "/"
            _ -> do
                user <- fetch currentUserId
                usersWithEmail <- query @User
                            |> filterWhere (#email, get #email user)
                            |> fetch
                case usersWithEmail of
                    -- This user already has an account
                    -- Delete this new account and then
                    -- log them into their previously created account
                    [user1, user2] -> do
                        let oldUser = find (\u -> get #id u /= get #id user) usersWithEmail
                        case oldUser of
                            Nothing -> do
                                Log.error $ ("We thought a user had two accounts, but something went wrong during setup userId: " :: Text) <> show (get #id user)
                                logout user
                                redirectToPath "/"
                            Just oldUser -> do
                                -- If the old account is not confirmed, then we should
                                -- delete it and use this new account instead.
                                -- if the new account is confirmed, use that account and 
                                -- delete the new one.
                                if not $ get #isConfirmed oldUser
                                    then do
                                        deleteInactiveUser oldUser
                                        render FinishUserSetupView { .. }
                                    else do
                                        logout user
                                        deleteInactiveUser user
                                        login oldUser
                                        redirectToPath "/"
                    -- This user has only one account, it needs to be setup
                    [user1] -> render FinishUserSetupView { .. }
                    -- This user has either more than two accounts, or no accounts
                    -- either way, something went wrong
                    _ -> do
                        Log.error $ ("There was a wrong number of accounts for a user in FinishUserSetupAction. userid: " :: Text) <> show (get #id user)
                        logout user
                        redirectToPath "/"

    action UpdateUserSetupAction = do
        user :: User <- fetch currentUserId
        user
            |> fill @'["username", "timezone"]
            |> validateField #timezone (isInList allTimezones)
            |> validateField #username (hasMaxLength usernameMaxLength |> withCustomErrorMessage "Your username must be shorter than 15 characters.")
            |> validateField #username (hasMinLength usernameMinLength |> withCustomErrorMessage "Your username must be atleast than 3 characters.")
            |> validateField #username isUsernameChars
            |> validateIsUnique #username
            >>= ifValid \case
                Left user -> render FinishUserSetupView { .. }
                Right user -> do
                    user <- user
                        |> set #isConfirmed True
                        |> set #isSetup True
                        |> updateRecord
                    setUserToFollowSelf (get #id user)
                    redirectToPath "/"


    action EditCurrentUserAction = do
        user <- fetch currentUserId
        render EditView { .. }

    -- Update a profile
    -- Users should only be able to edit themselves
    action UpdateUserAction { userId } = do

        -- TODO: We should just grab the current userid, there is no need to do this check
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)

        let profilePictureOptions = ImageUploadOptions
                { convertTo = "jpg"
                , imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"
                }
        user
            |> fill @["timezone","bio","pictureUrl"]
            |> validateField #bio (hasMaxLength 160)
            |> validateField #username (hasMaxLength usernameMaxLength |> withCustomErrorMessage "Your username must be shorter than 15 characters.")
            |> validateField #username (hasMinLength usernameMinLength |> withCustomErrorMessage "Your username must be atleast than 3 characters.")
            |> validateField #username isUsernameChars
            |> validateField #timezone (isInList allTimezones)
            |> uploadToStorage #pictureUrl
            >>= validateIsUnique #email
            >>= validateIsUnique #username
            >>= ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    user <- user |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditCurrentUserAction

    -- Create a user action
    -- Anybody can create a user
    action CreateUserAction = do
        -- Don't let loggedin users create new users
        case currentUserOrNothing of
            Just _ -> redirectToPath "/"
            Nothing -> do
                let user = newRecord @User
                user
                    |> fill @["email", "passwordHash", "timezone", "username"]
                    |> validateField #email isEmail
                    |> validateField #passwordHash nonEmpty
                    |> validateField #username (hasMaxLength usernameMaxLength |> withCustomErrorMessage "Your username must be shorter than 15 characters.")
                    |> validateField #username (hasMinLength usernameMinLength |> withCustomErrorMessage "Your username must be atleast than 3 characters.")
                    |> validateField #username isUsernameChars
                    |> validateField #timezone (isInList allTimezones)
                    |> validateIsUnique #email
                    >>= validateIsUnique #username
                    >>= ifValid \case
                        Left user -> render NewView { .. }
                        Right user -> do
                            hashed <- hashPassword (get #passwordHash user)
                            confirmationKey <- randomText
                            hashedConfirmationKey <- hashToken confirmationKey
                            user <- user
                                |> set #passwordHash hashed
                                |> set #confirmationKey (Just hashedConfirmationKey)
                                |> createRecord
                            setUserToFollowSelf (get #id user)
                            -- If we are in production, send an authentication email
                            -- Otherwise, just auto confirm and sign in
                            if True
                                then sendMail ConfirmationMail { user, confirmationKey }
                                else (user |> set #isConfirmed True
                                           |> updateRecord) >> pure ()
                            setSuccessMessage "We sent you an email!"
                            redirectToPath "/"

    action ConfirmUserEmailAction { userId, confirmationKey } = do
        -- Send users who are signed in home
        case currentUserOrNothing of
            Just _ -> redirectToPath "/"
            Nothing -> do
                -- Find user who is trying to confirm thier email
                user <- query @User
                    |> filterWhere (#id, userId)
                    |> fetchOneOrNothing

                case user of
                    Nothing -> failConfirm
                    Just user -> do
                        -- If the user is locked out of email confirms, that means they tried to confirm
                        -- too many times. Their unconfirmed account will eventually be deleted.
                        -- We don't want to delete right away because then they can just sign up again
                        -- and attempt to guess the confirmation key over and over.
                        if get #failedEmailConfirmAttempts user >= maxConfirmAttempts
                            then failConfirm
                            else do
                                -- Increment how many times someone tried to confirm this email.
                                user
                                    |> modify #failedEmailConfirmAttempts (+ 1)
                                    |> updateRecord

                                case get #confirmationKey user of
                                    Nothing -> failConfirm
                                    Just validconfirmationKeyHashed -> do
                                        if not $ verifyToken validconfirmationKeyHashed confirmationKey
                                            then failConfirm
                                            else do
                                                user
                                                    |> set #isConfirmed True
                                                    |> set #isSetup True
                                                    |> set #confirmationKey Nothing
                                                    |> updateRecord
                                                login user
                                                redirectToPath "/" -- TODO Redirect to page specific to new users
                    where
                        maxConfirmAttempts = 10
                        failConfirm = setErrorMessage failedToConfirmMessage >> redirectToPath "/"

                        -- Let's give the likely hackers the same error message for all errors
                        -- so they don't know how many chances they have before the timeout.
                        -- This is the only valid error message a real user might recive.
                        failedToConfirmMessage = "Sorry, we couldn't confirm your email. \
                                                \ Confirmation emails expire in 2 hours. \
                                                \ Try signing up again."

-- Every user follows themself, this makes things eaiser later
setUserToFollowSelf :: (?modelContext::ModelContext) => Id' "users" -> IO UserFollow
setUserToFollowSelf newUserId = newRecord @UserFollow
                    |> set #followerId newUserId
                    |> set #followedId newUserId
                    |> createRecord

-- Permanently delete a user
-- Warning: It's probably better to deactivate. TODO: Add deactivation
-- This will not work on a use who is active (a user with posts, likes, etc...)
deleteInactiveUser :: (?modelContext::ModelContext) => User -> IO ()
deleteInactiveUser userToDelete = do
    follows <- query @UserFollow
                |> queryOr
                    (filterWhere (#followerId, get #id userToDelete))
                    (filterWhere (#followedId, get #id userToDelete))
                |> fetch

    -- Delete all of the follows
    deleteRecords follows
    -- Delete the users
    deleteRecord userToDelete