module Web.View.Posts.Edit where
import Web.View.Prelude

data EditView = EditView { post :: Post }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>{get #createdOnDay post}</h1>
        {renderForm post}
    |]

renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {(textareaField #body) { disableLabel = True, fieldClass="edit-text-area" }}
    {submitButton}
|]
