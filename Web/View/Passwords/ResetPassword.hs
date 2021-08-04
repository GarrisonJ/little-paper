module Web.View.Passwords.ResetPassword where
import Web.View.Prelude

data ResetPasswordView = ResetPasswordView { resetToken :: Text, user :: User }

instance View ResetPasswordView where
    beforeRender view = do
        setLayout welcomePageLayout

    html ResetPasswordView { .. } = placeNextToWelcomeImage
        [hsx|
            <div class="h-100">
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <div class="w-100">
                        <div class="mx-auto mb-5">
                            Reset password for {get #email user}
                            {renderForm (get #id user) resetToken}
                        </div>
                    </div>
                </div>
            </div>
        |]


renderForm userId resetToken = [hsx|
    <form method="POST" action={CreateResetPasswordAction} id="" class="new-form">
        <div class="form-group" id="form-group-password">
            <input type="password" placeholder="New Password" name="password" id="password" class="form-control" />
        </div>
        <div class="form-group" id="form-group-userId">
            <input type="hidden" name="userId" id="userId" class="form-control" value={show userId}/>
        </div>
        <div class="form-group" id="form-group-resetToken">
            <input type="hidden" name="resetToken" id="resetToken" class="form-control" value={resetToken}/>
        </div>
        <button class="btn btn-primary btn-block">Reset Password</button>
    </form>
|]