module Web.View.Users.New where
import Web.View.Prelude
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data NewView = NewView { user :: User }

instance View NewView where
    beforeRender view = do
        setLayout basicLayout

    html NewView { .. } = [hsx|
        <script>
        $(document).on('ready turbolinks:load', function () {
                console.log("We made it")
                const tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
                document.getElementById('user_timezone').value = tz;
            });
        </script>
        {renderForm user}
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
    {submitButton}
|]