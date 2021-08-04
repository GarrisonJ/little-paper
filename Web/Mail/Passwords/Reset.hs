module Web.Mail.Passwords.Reset where
import Web.View.Prelude
import IHP.MailPrelude

data PasswordResetMail = PasswordResetMail { user :: User, resetToken :: Text }

instance BuildMail PasswordResetMail where
    subject = "Reset your password"
    to PasswordResetMail { .. } = Address { addressName = Nothing,
                                            addressEmail = get #email user }
    from = "no-reply@daily.computer"
    html PasswordResetMail { .. } = [hsx|
        Hey {get #username user}, <br/>

        Someone requested a new password for your https://Daily.computer account. <br/>

        <a href={resetPasswordUrl}>Reset Password!</a> <br/>

        If you didn't make this request, then you can ignore this email :)
        <br/><br/>
    |]
        where
            resetPasswordUrl = urlTo NewResetPasswordAction { userId = (get #id user), resetToken = resetToken}
