module Web.View.Users.Show where
import Web.View.Prelude
import Application.Helper.PostsQuery
import qualified Web.View.Posts.Show (renderPost)

data ShowView = ShowView {
                          user :: User
                        , posts :: [PostWithMeta]
                        , followed :: Bool
                        , likes :: [Like]
                        , followerCount :: Int
                        , postCount :: Int
                        }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="yosemite-window card text-center p-3">
            <h1>{get #username user}</h1>
            <img class="border rounded-circle mx-auto" src={picturePath} style="width:100px; height: 100px"/>
            <div class="d-flex justify-content-center">
                <div class="pr-2">{followerCount} <span class="text-muted pl-1">Followers</span></div>
                <div class="">{postCount} <span class="text-muted pl-1">Posts</span></div>
            </div>
            <div class="pt-3">
                {followButton}
            </div>
            <div class="pt-3">
                {get #bio user}
            </div>
        </div>
        <div class="my-3 p-3">
            {forEach (posts) (\post -> Web.View.Posts.Show.renderPost (isPostLiked post likes) post)}
        </div>
    |]
        where
            isPostLiked post likes = (get #id post) `elem` (fmap (get #postId) likes)
            followButton = if get #id user == get #id currentUser
                then [hsx||]
                else renderFollowButton followed user
            picturePath :: Text
            picturePath = case get #pictureUrl user of
                            Nothing -> "/space.jpeg"
                            Just url -> url

renderFollowButton :: Bool -> User -> Html
renderFollowButton followed user = formForWithOptions user followButtonFormOptions [hsx|
    {(hiddenField #id)}
    {submitButton { label= if followed then "Unfollow" else "Follow" }}
|]

followButtonFormOptions :: FormContext User -> FormContext User
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