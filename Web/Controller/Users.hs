module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.Index
import Web.View.Users.New
import Web.View.Users.Edit
import Web.View.Users.Show
import Web.Controller.Static

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
        let user = newRecord
        render NewView { .. }

    -- Show a users profile
    -- This should have no restrictions
    action ShowUserAction { userId } = do
        user <- fetch userId
        render ShowView { .. }

    -- Form to edit user
    -- Users should only be able to edit themselves
    action EditUserAction { userId } = do
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)
        render EditView { .. }

    -- Update a profile
    -- Users should only be able to edit themselves
    action UpdateUserAction { userId } = do
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)
        user
            |> buildUser
            |> ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    user <- user |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditUserAction { .. }

    -- Create a user action
    -- Anybody can create a user
    action CreateUserAction = do
        let user = newRecord @User
        user
            |> fill @["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
                    redirectToPath "/"

    -- Delete a user
    -- Only a user can delete themselves
    action DeleteUserAction { userId } = do
        user <- fetch userId
        accessDeniedUnless (get #id user == currentUserId)
        deleteRecord user
        setSuccessMessage "User deleted"
        redirectTo UsersAction

buildUser user = user
    |> fill @["email","passwordHash","failedLoginAttempts","timezone"]
