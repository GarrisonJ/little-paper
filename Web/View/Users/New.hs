module Web.View.Users.New where
import Web.View.Prelude
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)

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
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={UsersAction}>Users</a></li>
                <li class="breadcrumb-item active">New User</li>
            </ol>
        </nav>
        <h1>New User</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(passwordField #passwordHash) { fieldLabel = "Password"}}
    {(textField #username)}
    {(selectField #timezone allTimezones) { fieldLabel = "Prefered Timezone (You can change this later)"}}
    {submitButton}
|]
