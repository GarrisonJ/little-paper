module Web.View.Users.Edit where
import Web.View.Prelude
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)

data EditView = EditView { user :: User }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Settings</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(selectField #timezone allTimezones) { fieldLabel = "Prefered Timezone"}}
    {submitButton}
|]
