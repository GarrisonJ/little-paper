module Web.View.Posts.Index where
import Web.View.Prelude
import Web.View.Posts.Show

data IndexView = IndexView { posts :: [Include "userId" Post], todaysPost :: Maybe Post }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderPostInput}
        <div class="my-3 p-4 bg-body shadow-sm border">
            <div>{forEach posts renderPost}</div>
        </div>
    |]
        where
            renderPostInput = case todaysPost of
                    Just p -> [hsx|<h3>You posted something today. Nice job!</h3>|]
                    Nothing -> [hsx|
                                <div class="card bg-transparent border-light">
                                    <div class="card-header border-light">You have't posted today!</div>
                                    <div class="card-body border-light">
                                        {renderPostForm (newRecord :: Post)}
                                    </div>
                                </div>
                            |]

            renderPost post = [hsx|
                {Web.View.Posts.Show.renderPost (get #userId post) post}
                <hr>
            |]

renderPostForm :: Post -> Html
renderPostForm post = formForWithOptions post postFormOptions [hsx|
    {(textareaField #body) { disableLabel = True }}
    {submitButton { label= "Submit", buttonClass="float-right send-message-button" } }
|]

postFormOptions :: FormContext (Post) -> FormContext (Post)
postFormOptions formContext =
    formContext
    |> set #formAction (pathTo CreatePostAction)
