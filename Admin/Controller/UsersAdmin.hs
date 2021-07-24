module Admin.Controller.UsersAdmin where

import Admin.Controller.Prelude
import Admin.View.Users.Index
import Admin.View.Users.New
import Admin.View.Users.Edit
import Admin.View.Users.Show

instance Controller UsersAdminController where
    beforeAction =
        ensureIsAdmin @Admin

    action UsersAdminAction = do
        users <- query @User |> fetch
        render IndexView { .. }

    action NewUserAdminAction = do
        let user = newRecord
        render NewView { .. }

    action ShowUserAdminAction { userId } = do
        user <- fetch userId
        render ShowView { .. }

    action EditUserAdminAction { userId } = do
        user <- fetch userId
        render EditView { .. }

    action UpdateUserAdminAction { userId } = do
        user <- fetch userId
        user
            |> buildUser
            |> ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    user <- user |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditUserAdminAction { .. }

    action CreateUserAdminAction = do
        let user = newRecord @User
        user
            |> buildUser
            |> ifValid \case
                Left user -> render NewView { .. } 
                Right user -> do
                    user <- user |> createRecord
                    setSuccessMessage "User created"
                    redirectTo UsersAdminAction

    action DeleteUserAdminAction { userId } = do
        user <- fetch userId
        deleteRecord user
        setSuccessMessage "User deleted"
        redirectTo UsersAdminAction

buildUser user = user
    |> fill @["email","passwordHash","failedLoginAttempts","timezone","username","isconfirmed"]
