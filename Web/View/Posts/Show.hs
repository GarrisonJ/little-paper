module Web.View.Posts.Show where
import Web.View.Prelude

data ShowView = ShowView { post :: Include "userId" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="my-3 p-3 bg-body rounded shadow-sm">
        {renderPost (post |> get #userId) post}
        </div>

    |]

renderPost user post = [hsx|
    <div class="d-flex text-muted pt-3">
        <a href={ShowProfileAction username}>
            <img class="d-placeholder-img flex-shrink-0 me-2 rounded" src={picturePath} style="width:50px; height: 50px"/>
        </a>
        <p class="pb-3 mb-0 small lh-sm border-bottom" style="width: 100%;">
            <strong class="d-block text-gray-dark">
                <a href={ShowProfileAction username}>@{username}</a>

                <a class="text-muted float-right" href={ShowPostAction (get #id post)}>
                    {get #createdOnDay post}
                </a>
            </strong>
            {get #body post}
            <div class="dropdown float-right">
                <button class="btn" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                    {kebabHorizontal}
                </button>
                <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
                    <a class="dropdown-item" href={EditPostAction (get #id post)}>Edit</a>
                    <a class="dropdown-item js-delete text-muted" href={DeletePostAction (get #id post)}>Delete</a>
                </div>
            </div>
        </p>
    </div>
|]
    where
        username = user |> get #username
        kebabHorizontal = [hsx|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="16" height="16"><path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path></svg>|]
        picturePath :: Text
        picturePath = case get #pictureUrl user of
                        Nothing -> "/space.jpeg"
                        Just url -> url