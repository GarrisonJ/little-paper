module Web.View.Users.FinishUserSetup where
import Web.View.Prelude
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data FinishUserSetupView = FinishUserSetupView { user :: User }

instance View FinishUserSetupView where
    beforeRender view = do
        setLayout welcomePageLayout

    html FinishUserSetupView { .. } = placeNextToWelcomeImage
        [hsx|
            <div class="h-100">
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <div class="w-100">
                        <div class="mx-auto mb-5">
                            <h5>What should people call you?</h5>
                            {renderForm user}
                        </div>
                    </div>
                </div>
            </div>
            <script>
            $(document).on('ready turbolinks:load', function () {
                    const tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
                    document.getElementById('user_timezone').value = tz;
            });
            </script>
        |]

renderForm :: User -> Html
renderForm user = formForWithOptions user usernameFormOptions [hsx|
    <div class="input-group mb-3">
        <span class="input-group-text" id="basic-addon1">@</span>
        {(textField #username) { disableLabel=True, disableGroup=True, placeholder="Username" }}
    </div>
    {(hiddenField #timezone)}
    <div class="form-check">
        <input class="form-check-input" type="checkbox" value="" id="termsOfService" required="required">
        <label class="form-check-label" for="termsOfService">
            I agree to the <a  target="_blank" href={urlTo TermsAction}>terms of service</a>
        </label>
    </div>
    <div class="d-grid mx-auto">
        {submitButton {label="Set Username", buttonClass="btn rainbow-button"}}
    </div>
|]


usernameFormOptions :: FormContext User -> FormContext User
usernameFormOptions formContext =
    formContext
    |> set #formAction (pathTo UpdateUserSetupAction)
