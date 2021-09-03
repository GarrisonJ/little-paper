module Web.View.Posts.Show where
import Web.View.Prelude
import Application.Helper.PostsQuery

data ShowView = ShowView {
                           post :: PostWithMeta
                         , isLiked :: Bool
                         , comments :: [Include "userId" Comment]
                         , commentspagination :: Pagination
                         }


instance View ShowView where
    html ShowView { .. } = [hsx|
        <div>
            {renderPost isLiked post}
        </div>
        <div class="m-3 p-2 d-flex yosemite-window">
            <div class="col">
                <div class="">
                    {renderFormComment newComment}
                </div>
                {forEach comments (\c -> renderComment (c |> get #userId) c)}
                {renderPagination commentspagination}
            </div>
        </div>
    |]
        where
            newComment = newRecord @Comment
                            |> set #postId (get #id post)

renderFormComment :: Comment -> Html
renderFormComment comment = formFor comment [hsx|
    {(hiddenField #postId)}
    {(textField #body) {disableLabel = True}}
    {submitButton {buttonClass="float-right", label="Reply"}}
|]

renderComment user comment = [hsx|
    <div class="w-100 row">
        <div class="p-2 w-100">
            <a href={ShowProfileAction username}>
                <img class="border rounded-circle" src={picturePath} style="width:50px; height: 50px"/>
            </a>
            <a class="p-2" href={ShowProfileAction username}>
                {username}
            </a>
            {timeAgo (get #createdAt comment)}
            <div class="float-right">
                {renderControlDropdown user}
            </div>
        </div>
        <p class="p-3 post-text w-100">
            {get #body comment}
        </p>
    </div>
    <hr>
|]
    where
        username = user |> get #username
        picturePath :: Text
        picturePath = case get #pictureUrl user of
                        Nothing -> "/space.jpeg"
                        Just url -> url
        renderControlDropdown post =
                if (get #id currentUser) == get #id user
                    then userDropdown
                    else [hsx||]
        userDropdown = [hsx|
            <div class="dropdown float-right">
                <button class="btn" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                    {kebabHorizontal}
                </button>
                <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
                    <a class="dropdown-item js-delete text-muted" href={DeleteCommentAction (get #id comment)}>Delete</a>
                </div>
            </div>
        |]

renderPost isLiked post = [hsx|
    <div class="d-flex yosemite-window">
        <div class="w-100">
            <div class="p-2 ">
                <a href={ShowProfileAction username}>
                    <img class="border rounded-circle" src={picturePath} style="width:50px; height: 50px"/>
                </a>
                <a class="p-2" href={ShowProfileAction username}>
                    {username}
                </a>
                <div class="float-right">
                <div class="like-button" style={if isLiked then "color:#ff5e57;" :: String else ""} data-postid={tshow (get #id post)} data-url={CreateLikeAction}>
                    <span class="likes-counter">
                        {get #likesCount post}
                    </span>
                    {heartIcon}
                </div>
                </div>
                <small class="text-muted">
                    <a href={ShowPostAction (get #id post)}>
                        {get #createdOnDay post}
                    </a>
                </small>
            </div>
            {renderControlDropdown}
            <p class="pl-3 post-text">
                {get #body post}
            </p>
            <div class="pl-3 pb-2">
            <a href={ShowPostAction (get #id post)} class="">
             {chatIcon} {get #commentsCount post}
            </a>
            </div>
        </div>
    </div>
|]
    where
        username = get #username post
        picturePath :: Text
        picturePath = case get #pictureUrl post of
                        Nothing -> "/space.jpeg"
                        Just url -> url
        renderControlDropdown =
                    if (get #id currentUser) == get #userId post
                        then userDropdown
                        else [hsx||]
        userDropdown = [hsx|
            <div class="dropdown float-right">
                <button class="btn" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                    {kebabHorizontal}
                </button>
                <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
                    <a class="dropdown-item" href={EditPostAction (get #id post)}>Edit</a>
                    <a class="dropdown-item js-delete text-muted" href={DeletePostAction (get #id post)}>Delete</a>
                </div>
            </div>
        |]



kebabHorizontal = [hsx|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="16" height="16">
  <path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path>
</svg>|]

chatIcon = [hsx|
<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-chat-left" viewBox="0 0 16 16">
  <path d="M14 1a1 1 0 0 1 1 1v8a1 1 0 0 1-1 1H4.414A2 2 0 0 0 3 11.586l-2 2V2a1 1 0 0 1 1-1h12zM2 0a2 2 0 0 0-2 2v12.793a.5.5 0 0 0 .854.353l2.853-2.853A1 1 0 0 1 4.414 12H14a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2z"/>
</svg>
|]

heartIcon = [hsx|
<svg xmlns="http://www.w3.org`/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-heart-fill" viewBox="0 0 16 16">
    <path fill-rule="evenodd" d="M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"/>
</svg>
|]