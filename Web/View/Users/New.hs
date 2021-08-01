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
                            <h1><a href="/">Daily</a></h1>
                            <h5>Sign up today! 🎉</h5>
                            {renderForm user}
                            <div class="text-center m-2">
                                <a href={NewSessionAction}>Log in</a>
                            </div>
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
    {(textField #email) {disableLabel=True, placeholder="Email"}}
    {(passwordField #passwordHash) { fieldLabel = "Password", disableLabel=True, placeholder="Password"}}
    <div class="input-group mb-3">
        <span class="input-group-text" id="basic-addon1">@</span>
        {(textField #username) { disableLabel=True, disableGroup=True, placeholder="Username" }}
    </div>
    {(hiddenField #timezone)}
    <div class="d-grid mx-auto">
        {submitButton {label="Sign up", buttonClass="btn rainbow-button"}}
    </div>
|]