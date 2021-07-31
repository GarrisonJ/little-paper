module Web.Mail.Users.Confirmation where
import Web.View.Prelude
import IHP.MailPrelude

data ConfirmationMail = ConfirmationMail { user :: User }

instance BuildMail ConfirmationMail where
    subject = "Confirm your Account"
    to ConfirmationMail { .. } = Address { addressName = Nothing,
                                           addressEmail = get #email user }
    from = "no-reply@daily.computer"
    html ConfirmationMail { .. } = [hsx|
        Hey {get #username user}, <br/>
        Thanks for signing up! Please confirm your account by following this link:
        <a href={confirmUrl}>Confirm</a>
        <br /><br />
    |]
        where
            confirmUrl = urlTo ConfirmUserEmailAction { userId = (get #id user), confirmationKey=(get #confirmationKey user)}
