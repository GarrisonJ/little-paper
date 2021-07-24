module Admin.View.Users.New where
import Admin.View.Prelude

data NewView = NewView { user :: User }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={UsersAdminAction}>Users</a></li>
                <li class="breadcrumb-item active">New User</li>
            </ol>
        </nav>
        <h1>New User</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(textField #passwordHash)}
    {(textField #failedLoginAttempts)}
    {(textField #timezone)}
    {(textField #username)}
    {(textField #isconfirmed)}
    {submitButton}
|]
