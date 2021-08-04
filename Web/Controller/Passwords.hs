module Web.Controller.Passwords where

import Web.Controller.Prelude
import Web.View.Passwords.ForgotPassword
import Web.View.Passwords.ResetPassword
import Web.Mail.Passwords.Reset

instance Controller PasswordsController where

    -- Show the page where the user can submit their email
    -- so we can send them a password reset link
    action NewForgotPasswordAction = do
        render ForgotPasswordView

    -- Create the password reset key and send it via email
    action CreateForgotPasswordAction = do
        let email = param @Text "email"
        -- Always say the same thing
        setSuccessMessage "We sent you an email!"

        -- Check if the submitted a valid email
        if isFailure $ isEmail email
            then do
                setErrorMessage "Please enter a valid email"
                redirectToPath "/"
            else do
                user <- query @User
                    |> filterWhere (#email, email)
                    |> fetchOneOrNothing
                case user of
                    Nothing -> redirectToPath "/"
                    Just user -> do
                        -- Delete the old password resets if there are any
                        deleteOldPasswordResets user

                        -- Get a random UUID and convert it to Text to use as our key
                        resetToken <- randomText

                        -- Hash the reset token to save in our database
                        hashedRandomToken <- hashToken resetToken

                        -- Save password reset record
                        newRecord @PasswordReset
                            |> set #userId (get #id user)
                            |> set #resetToken hashedRandomToken
                            |> createRecord

                        -- Email user the reset token
                        sendMail PasswordResetMail { .. }
                        redirectToPath "/"
        where
           deleteOldPasswordResets user = do
                oldPasswordResets <- query @PasswordReset
                                        |> filterWhere (#userId, get #id user)
                                        |> fetch
                deleteRecords oldPasswordResets

    -- Get the password reset page, the user gets to this page with
    -- the link sent to their email
    action NewResetPasswordAction { userId, resetToken } = do
        passwordReset <- query @PasswordReset
                            |> filterWhere (#userId, userId)
                            |> fetchOneOrNothing

        case passwordReset of
            Nothing -> redirectToPath "/"
            Just passwordReset -> do
                if not $ verifyToken (get #resetToken passwordReset) resetToken
                    then redirectToPath "/"
                    else do
                        user <- fetch (get #userId passwordReset)
                        render ResetPasswordView { .. }

    -- Performs the reset
    action CreateResetPasswordAction = do
        let resetToken = param @Text "resetToken"
        let newPassword = param @Text "password"
        let userId = textToId (param @Text "userId") :: Id User

        passwordReset <- query @PasswordReset
                            |> filterWhere (#userId, userId)
                            |> fetchOneOrNothing

        case passwordReset of
            Nothing -> pure ()
            Just passwordReset -> do
                if not $ verifyToken (get #resetToken passwordReset) resetToken
                    then pure ()
                    else do
                        user <- query @User
                            |> filterWhere (#id, (get #userId passwordReset))
                            |> fetchOneOrNothing
                        case user of
                            Nothing -> pure ()
                            Just user -> do
                                hashed <- hashPassword newPassword
                                user 
                                    |> set #passwordHash hashed
                                    |> updateRecord
                                deleteRecord passwordReset
                                pure ()
                        
        setSuccessMessage "Your new password has been set"
        redirectToPath "/"

