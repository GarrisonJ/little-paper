module Web.View.Posts.Show where
import Web.View.Prelude

data ShowView = ShowView {
                           post :: Include "userId" Post
                         , isLiked :: Bool
                         }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="my-3 p-3 bg-body rounded shadow-sm">
        {renderPost (post |> get #userId) isLiked post}
        </div>
    |]

renderPost user isLiked post = [hsx|
    <div class="d-flex">
        <div class="w-100 border border-top-0">
            <div class="p-2 ">
                <a href={ShowProfileAction username}>
                    <img class="border rounded-circle" src={picturePath} style="width:50px; height: 50px"/>
                </a>
                <a class="p-2" href={ShowProfileAction username}>
                    {username}
                </a>
                <div class="float-right">
                <div class="like-button" style={if isLiked then "color:red;" :: String else ""} data-postid={tshow (get #id post)} data-url={CreateLikeAction}>
                    <svg xmlns="http://www.w3.org`/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-heart-fill" viewBox="0 0 16 16">
                        <path fill-rule="evenodd" d="M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"/>
                    </svg>
                </div>
                </div>
                <small class="text-muted">
                    <a href={ShowPostAction (get #id post)}>
                        {get #createdOnDay post}
                    </a>
                </small>
            </div>
            {renderControlDropdown user}
            <p class="p-3 post-text">
                {get #body post}
            </p>
        </div>
    </div>
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
                    <a class="dropdown-item" href={EditPostAction (get #id post)}>Edit</a>
                    <a class="dropdown-item js-delete text-muted" href={DeletePostAction (get #id post)}>Delete</a>
                </div>
            </div>
        |]
        kebabHorizontal = [hsx|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="16" height="16"><path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path></svg>|]