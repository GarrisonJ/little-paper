module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Index
import Web.View.Users.New
import Web.View.Users.Edit
import Web.View.Users.Show
import Web.Controller.Static
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)
import Text.Regex.TDFA
import Web.Mail.Users.Confirmation

instance Controller UsersController where

    -- List all of the users.
    -- TODO: This should be restricted to admins
    action UsersAction = do
        ensureIsUser
        users <- query @User |> fetch
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

    action EditCurrentUserAction = do
        user <- fetch currentUserId
        render EditView { .. }

    -- Update a profile
    -- Users should only be able to edit themselves
    action UpdateUserAction { userId } = do
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)

        let profilePictureOptions = ImageUploadOptions
                { convertTo = "jpg"
                , imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"
                }
        user
            |> buildUser
            |> uploadImageWithOptions profilePictureOptions #pictureUrl
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
                    |> validateField #username ((hasMaxLength 15) |> withCustomErrorMessage "Your username must be shorter than 15 characters.")
                    |> validateField #username isUsernameChars
                    |> validateField #timezone (isInList allTimezones)
                    |> validateIsUnique #email
                    >>= validateIsUnique #username
                    >>= ifValid \case
                        Left user -> render NewView { .. }
                        Right user -> do
                            hashed <- hashPassword (get #passwordHash user)
                            user <- user
                                |> set #passwordHash hashed
                                |> createRecord
                            setUserToFollowSelf (get #id user)
                            setSuccessMessage "You have registered successfully"
                            redirectToPath "/"
                where
                    -- Every user follows themself, this makes things eaiser later
                    setUserToFollowSelf newUserId = newRecord @UserFollow
                                        |> set #followerId newUserId
                                        |> set #followedId newUserId
                                        |> createRecord

                    isUsernameChars :: Text -> ValidatorResult
                    isUsernameChars text | text =~ ("([A-Za-z0-9\\_]+)" :: Text) = Success
                    isUsernameChars text = Failure "Your username can only contain letters, numbers and '_'"

    action CreateFollowAction = do
        let userId = param @(Id User) "id"

        follow <- query @UserFollow
            |> filterWhere (#followerId, currentUserId)
            |> filterWhere (#followedId, userId)
            |> fetchOneOrNothing

        case follow of
            Just f -> do
                deleteRecord f
                setSuccessMessage "Unfollowed"
            Nothing -> newRecord @UserFollow
                |> set #followerId currentUserId
                |> set #followedId userId
                |> ifValid \case
                    Left _ -> setSuccessMessage "Something happened"
                    Right follow -> do
                        follow |> createRecord
                        setSuccessMessage "Followed"

        user <- fetch userId
        let username = get #username user
        redirectToPath $ "/user/" <> username

buildUser user = user
    |> fill @["email","passwordHash","failedLoginAttempts","timezone", "username", "pictureUrl"]
