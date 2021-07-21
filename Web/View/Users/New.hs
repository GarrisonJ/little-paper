module Web.View.Users.New where
import Web.View.Prelude
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data NewView = NewView { user :: User }

instance View NewView where
    beforeRender view = do
        setLayout welcomePageLayout

    html NewView { .. } = placeNextToWelcomeImage
        [hsx|
            <div class="h-100">
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <div class="w-100">
                        <div class="mx-auto mb-5">
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
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(passwordField #passwordHash) { fieldLabel = "Password"}}
    <label>Username</label>
    <div class="input-group mb-3">
        <span class="input-group-text" id="basic-addon1">@</span>
        {(textField #username) { disableLabel=True, disableGroup=True }}
    </div>
    {(selectField #timezone allTimezones) { fieldLabel = "Prefered Timezone (You can change this later)"}}
    <div class="d-grid mx-auto">
        {submitButton {label="Signup", buttonClass="btn rainbow-button"}}
    </div>
|]