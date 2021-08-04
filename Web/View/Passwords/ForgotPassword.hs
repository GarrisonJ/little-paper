module Web.View.Passwords.ForgotPassword where
import Web.View.Prelude

data ForgotPasswordView = ForgotPasswordView {}

instance View ForgotPasswordView where
    beforeRender view = do
        setLayout welcomePageLayout

    html ForgotPasswordView = placeNextToWelcomeImage
        [hsx|
            <div class="h-100">
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <div class="w-100">
                        <div class="mx-auto mb-5">
                            <h5>Forgot your password? We'll send you a link to reset it.</h5>
                            {renderForm}
                            <div class="text-center m-2">
                                <a href={NewSessionAction}>Nevermind</a>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        |]


renderForm :: Html
renderForm = [hsx|
    <form method="POST" action={CreateForgotPasswordAction} id="" class="new-form">
        <div class="form-group" id="form-group-email">
            <input type="email" placeholder="Email" name="email" id="email" class="form-control" />
        </div>
        <button class="btn btn-primary btn-block">Send Password Reset</button>
    </form>
|]