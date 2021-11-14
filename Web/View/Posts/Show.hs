module Web.View.Posts.Show where
import Web.View.Prelude
import Application.Helper.PostsQuery
import Text.XML.Unresolved (renderBuilder)
import qualified Admin.Controller.Prelude as Data.Maybe
import Generated.Types (Post'(bigPostTitle))
import Web.View.Layout (wideLayout)
import Application.Helper.PostsQuery (PostWithMeta(isBigPost))

data ShowView = ShowView {
                           post :: PostWithMeta
                         , isLiked :: Bool
                         , comments :: [Include "userId" Comment]
                         , newComment :: Comment
                         , commentspagination :: Pagination
                         , user :: User
                         , followed :: Bool
                         , followerCount :: Int
                         }


instance View ShowView where
    beforeRender view = do
        let thePost = post view
        setTitle $ fromMaybe "Daily" $ get #bigPostTitle thePost
        when (get #isBigPost thePost) $ setLayout wideLayout

    html ShowView { .. } = [hsx|
        <div class="mb-5">
            {renderIfSmallPost post}
            {renderBigPostHeader}
            {renderBigPostBody}
            {renderComments comments commentspagination newComment}
        </div>
    |]
        where
            renderIfSmallPost post = if get #isBigPost post 
                then mempty
                else renderPost isLiked post
            
            renderBigPostHeader = if not (get #isBigPost post) 
                then mempty 
                else [hsx|
                <div class="container col-md-11 ">
                    <div class="justify-content-left">
                        <div class="justify-content-left d-flex">
                            <div class="col-10">
                                <h1 class="display-4">
                                    {fromMaybe "" $ get #bigPostTitle post}
                                </h1>
                                <div class="justify-content-left d-flex">
                                    <blockquote class="blockquote">
                                        <p class="text-muted mb-0">
                                            <em>{get #body post}</em>
                                        </p>
                                    </blockquote>
                                </div>
                            </div>
                            <div class="col-2">
                                <div class="float-right mt-4">
                                    <div class="like-button" style={if isLiked then "color:#ff5e57;" :: String else ""} data-postid={tshow (get #id post)} data-url={CreateLikeAction}>
                                        <span class="likes-counter">
                                            {get #likesCount post}
                                        </span>
                                        {heartIcon}
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="d-flex justify-content-left">
                        <div class="p-3">
                            <div class="d-flex align-items-center">
                                <a href={ShowProfileAction $ get #username user}>
                                    <div class="image">
                                        <img class="border rounded-circle" src={picturePath} style="width:60px; height: 60px">
                                    </div>
                                </a>
                                <div class="ml-3 w-100">
                                    <a class="text-reset" href={ShowProfileAction $ get #username user}>
                                        <h4 class="mb-0 mt-0">by {get #username user}</h4> 
                                    </a>
                                    <div class="mb-0 mt-0">{formatTime defaultTimeLocale "%A, %B %d, %Y" $ get #createdOnDay post}</div>
                                </div>
                                <div class="ml-3 mb-0 mt-0">{followButton}</div>
                            </div>
                        </div>
                    </div>
                </div>
            |]
            renderBigPostBody = case get #bigPostBody post of
                Nothing -> [hsx||]
                Just body -> [hsx|
                            <div class="d-flex yosemite-window col-12 col-md-11">
                                <div class="w-100 p-4 ql-editor">
                                    {preEscapedToHtml body}
                                </div>
                            </div>
                        |]

            picturePath :: Text
            picturePath = fromMaybe "/space.jpeg" (get #pictureUrl user)

            followButton =
                case currentUserOrNothing of
                        Nothing -> followButtonHtml
                        Just currentUser' -> if get #id user == get #id currentUser'
                                                then [hsx||]
                                                else followButtonHtml
            followButtonHtml = [hsx|
                                <button class={"btn follow-button " ++ followButtonTextClass}
                                        data-userid={tshow (get #id user)} 
                                        data-url={CreateFollowAction}>
                                        {followButtonText}
                                </button>
                                |]
            followButtonTextClass :: Text
            followButtonTextClass = if followed then "btn-light" else "btn-primary"

            followButtonText :: Text
            followButtonText = if followed then "Unfollow" else "Follow"


renderComments comments commentspagination newComment = if null comments && not isUserLoggedIn
    then [hsx||]
    else [hsx|
        <div class="justify-content-center">
            <div class="p-3 d-flex yosemite-window col-11 col-md-10">
                <div class="col">
                    {renderCommentInput}
                    {forEach comments (\c -> renderComment (c |> get #userId) c)}
                </div>
            </div>
            {renderCommentsPagination}
        </div>
    |]
    where
        renderCommentsPagination = if null comments
                            then [hsx||]
                            else [hsx|
                                <div class="justify-content-center p-2 pt-3 d-flex yosemite-window col-11 col-md-10">
                                    {renderPagination commentspagination}
                                </div>
                            |]
        isUserLoggedIn = case currentUserOrNothing of
                            Nothing -> False
                            Just _ -> True

        renderCommentInput = case currentUserOrNothing of
                            Nothing -> [hsx||]
                            Just _ -> renderFormComment newComment

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
|]
    where
        username = user |> get #username
        picturePath = fromMaybe "/space.jpeg" (get #pictureUrl user)
        renderControlDropdown post =
                case currentUserOrNothing of
                        Nothing -> [hsx||]
                        Just currentUser' -> if get #id currentUser' == get #id user
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

renderPost isLiked post = if get #isBigPost post
    then renderBigPost isLiked post
    else renderSmallPost isLiked post

renderBigPost isLiked post = [hsx|
    <div class="d-flex yosemite-window big-post">
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
                    {renderControlDropdown}
                </small>
            </div>
            <a class="text-reset" href={ShowPostAction (get #id post)}>
                <h2 class="pl-3">
                    {bigPostTitle}
                </h2>
                <blockquote class="pl-3 pr-3 blockquote">
                    <p class="text-muted mb-0">{get #body post}
                    </p>
                </blockquote>
            </a>
            <div class="pl-3 pb-2">
            <a href={ShowPostAction (get #id post)} class="">
             {chatIcon} {get #commentsCount post}
            </a>
            </div>
        </div>
    </div>
|]
    where
        bigPostTitle = Data.Maybe.fromMaybe "No Title" (get #bigPostTitle post)

        username = get #username post
        picturePath = fromMaybe "/space.jpeg" (get #pictureUrl post)
        renderControlDropdown =
                case currentUserOrNothing of
                        Nothing -> [hsx||]
                        Just currentUser' -> if get #id currentUser' == get #userId post
                                                then userDropdown
                                                else [hsx||]
        userDropdown = [hsx|
            <div class="dropdown d-inline">
                <button class="btn" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                    {kebabHorizontal}
                </button>
                <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
                    <a class="dropdown-item js-delete text-muted" href={DeletePostAction (get #id post)}>Delete</a>
                </div>
            </div>
        |]

renderSmallPost isLiked post = [hsx|
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
                    {renderControlDropdown}
                </small>
            </div>
            <a class="text-reset" href={ShowPostAction (get #id post)}>
                <p class="pl-3 pr-3 post-text">
                    {get #body post}
                </p>
            </a>
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
        picturePath = fromMaybe "/space.jpeg" (get #pictureUrl post)
        renderControlDropdown =
                case currentUserOrNothing of
                        Nothing -> [hsx||]
                        Just currentUser' -> if get #id currentUser' == get #userId post
                                                then userDropdown
                                                else [hsx||]
        userDropdown = [hsx|
            <div class="dropdown d-inline">
                <button class="btn" type="button" id="dropdownMenuButton" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                    {kebabHorizontal}
                </button>
                <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
                    <a class="dropdown-item js-delete text-muted" href={DeletePostAction (get #id post)}>Delete</a>
                </div>
            </div>
        |]



kebabHorizontal = [hsx|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="16" height="16" fill="currentColor">
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