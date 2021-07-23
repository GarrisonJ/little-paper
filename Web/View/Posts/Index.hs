module Web.View.Posts.Index where
import Web.View.Prelude
import Web.View.Posts.Show

data IndexView = IndexView { posts :: [Include "userId" Post], todaysPost :: Maybe Post }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderPostInput}
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Post</th>
                    </tr>
                </thead>
                <tbody>{forEach posts (\p ->Web.View.Posts.Show.renderPost (get #userId p) p)}</tbody>
            </table>
        </div>
    |]
        where
            renderPostInput = case todaysPost of
                    Just p -> [hsx|<h3>You posted something today. Nice job!</h3>|]
                    Nothing -> [hsx|
                                <div class="card bg-light">
                                <div class="card-header">You have't posted today!</div>
                                <div class="card-body">
                                {renderPostForm (newRecord :: Post)}
                                </div>
                                </div>
                            |]


renderPostForm :: Post -> Html
renderPostForm post = formForWithOptions post postFormOptions [hsx|
    {(textareaField #body) { disableLabel = True }}
    {submitButton { label= "Submit"} }
|]

postFormOptions :: FormContext (Post) -> FormContext (Post)
postFormOptions formContext =
    formContext
    |> set #formAction (pathTo CreatePostAction)
