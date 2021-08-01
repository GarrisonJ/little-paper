module Web.View.Posts.New where
import Web.View.Prelude

data NewView = NewView { post :: Post,
                         day :: Day,
                         dailyPost :: Maybe Post }

instance View NewView where
    html NewView { .. } = [hsx|
        <h3>{day}</h3>
        {renderForm post}
    |]

renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {(textareaField #body) { disableLabel = True }}
    {submitButton}
|]
