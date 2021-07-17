module Web.View.Posts.Index where
import Web.View.Prelude

data IndexView = IndexView { posts :: [Include "userId" Post], todaysPost :: Maybe Post }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderPostInput}

        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Post</th>
                        <th></th>
                        <th></th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach posts renderPost}</tbody>
            </table>
        </div>
    |]
        where
            renderPostInput = case todaysPost of
                    Just p -> [hsx|<h3>You posted something today! Nice Job!</h3>|]
                    Nothing -> [hsx|
                                <div>You have't posted today!</div>
                                {renderPostForm (newRecord :: Post)}
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



renderPost :: Include "userId" Post -> Html
renderPost post = [hsx|
    <tr>
        <td>{get #body post}</td>
        <td>{get #createdOnDay post}</td>
        <td><a href={ShowProfileAction username}>{username}</a></td>
        <td><a href={ShowPostAction (get #id post)}>Show</a></td>
        <td><a href={EditPostAction (get #id post)} class="text-muted">Edit</a></td>
        <td><a href={DeletePostAction (get #id post)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
    where username = post |> get #userId |> get #username