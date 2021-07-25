module Web.View.Users.Show where
import Web.View.Prelude
import qualified Web.View.Posts.Show (renderPost)

data ShowView = ShowView { user :: Include "posts" User, followed :: Bool }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>{get #username user} {followButton}</h1>
        <div class="my-3 p-3 bg-body rounded shadow-sm">
            {forEach (user |> get #posts) (Web.View.Posts.Show.renderPost user)}
        </div>
    |]
        where
            followButton = if get #id user == get #id currentUser
                then [hsx||]
                else renderFollowButton followed user


renderFollowButton :: Bool -> Include "posts" User -> Html
renderFollowButton followed user = formForWithOptions user followButtonFormOptions [hsx|
    {(hiddenField #id)}
    {submitButton { label= if followed then "Unfollow" else "Follow" }}
|]

followButtonFormOptions :: FormContext (Include "posts" User) -> FormContext (Include "posts" User)
followButtonFormOptions formContext =
    formContext
    |> set #formAction (pathTo CreateFollowAction)
    |> modify #formClass (\classes -> classes <> " form-inline follow-button")

renderPost :: Post -> Html
renderPost post = [hsx|
    <tr>
        <td>{get #body post}</td>
        <td>{get #createdOnDay post}</td>
    </tr>
|]