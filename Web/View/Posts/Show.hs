module Web.View.Posts.Show where
import Web.View.Prelude
import Application.Helper.PostsQuery
import Text.XML.Unresolved (renderBuilder)
import qualified Admin.Controller.Prelude as Data.Maybe
import Generated.Types (Post'(bigPostTitle))
import Web.View.Layout (wideLayout)

data ShowView = ShowView {
                           post :: PostWithMeta
                         , isLiked :: Bool
                         , comments :: [Include "userId" Comment]
                         , newComment :: Comment
                         , commentspagination :: Pagination
                         , likes :: [Include "userId" Like]
                         , user :: User
                         , followed :: Bool
                         , followerCount :: Int
                         , today :: Day
                         }


instance View ShowView where
    beforeRender view = do
        let thePost = post view
        setTitle $ fromMaybe "Daily" $ get #bigPostTitle thePost
        when (get #isBigPost thePost) $ setLayout wideLayout

    html ShowView { .. } = [hsx|
        <div class="mb-5">
            {renderIfSmallPost today post}
            {renderBigPostHeader}
            {renderBigPostBody}
            {renderActivity likes comments commentspagination newComment}
        </div>
    |]
        where
            renderIfSmallPost today post = if get #isBigPost post
                then mempty
                else renderPost today isLiked post
            
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
                                        <img class="border rounded-circle" src={picturePath (get #pictureUrl user)} style="width:60px; height: 60px">
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


renderLikes likes = [hsx|
    <div class="pl-4 pt-3">
        {forEach likes renderLike}
    </div>
|]

renderLike like = [hsx|
    <div class="mb-2">
        <a class="text-reset d-flex align-items-center" href={ShowProfileAction username}>
            <div class="image">
                <img class="border rounded-circle"
                     src={picturePath (get #userId like |> get #pictureUrl)}
                     style="width:60px; height: 60px">
            </div>
            <div class="ml-2">
                <span class="m-2">{username}</span><br/>
                <span class="m-2">{bio}</span>
            </div>
        </a>
    </div>
|]
    where
        username = get #userId like |> get #username
        bio = get #userId like |> get #bio


renderActivity likes comments commentspagination newComment = if null comments && not isUserLoggedIn
    then [hsx||]
    else [hsx|
        <div class="justify-content-center">
            <div class="p-3 d-flex yosemite-window col-11 col-md-10">
                <div class="col">
                    <ul class="nav nav-tabs" id="myTab" role="tablist">
                        <li class="nav-item">
                            <a class="nav-link active"
                            id="comments-tab"
                            data-toggle="tab"
                            href="#comments"
                            role="tab"
                            aria-controls="comments"
                            aria-selected="true">Comments</a>
                        </li>
                        <li class="nav-item">
                            <a class="nav-link"
                            id="likes-tab"
                            data-toggle="tab"
                            href="#likes"
                            role="tab"
                            aria-controls="likes"
                            aria-selected="false">{heartIcon} Liked By</a>
                        </li>
                    </ul>
                    <div class="tab-content" id="myTabContent">
                        <div class="tab-pane fade show active" id="comments" role="tabpanel" aria-labelledby="comments-tab">
                            {renderCommentInput}
                            {forEach comments (\c -> renderComment (c |> get #userId) c)}
                        </div>
                        <div class="tab-pane fade" id="likes" role="tabpanel" aria-labelledby="likes-tab">
                            {renderLikes likes}
                        </div>
                    </div>
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
                <img class="border rounded-circle" src={picturePath (get #pictureUrl user)} style="width:50px; height: 50px"/>
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

renderPost today isLiked post = if get #isBigPost post
    then renderBigPost today isLiked post
    else renderSmallPost today isLiked post

renderBigPost today isLiked post = [hsx|
    <div class="d-flex yosemite-window big-post">
        <div class="w-100">
            <div class="p-2 ">
                <a href={ShowProfileAction username}>
                    <img class="border rounded-circle" src={picturePath (get #pictureUrl post)} style="width:50px; height: 50px"/>
                </a>
                <a class="pl-2" href={ShowProfileAction username}>
                    {username}
                </a>
                ·
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
                        {getCreatedOnDayFormated today post :: Text}
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

renderSmallPost today isLiked post = [hsx|
    <div class="d-flex yosemite-window">
        <div class="w-100">
            <div class="p-2">
                <a href={ShowProfileAction username}>
                    <img class="border rounded-circle" src={picturePath (get #pictureUrl post)} style="width:50px; height: 50px"/>
                </a>
                <a class="pl-2" href={ShowProfileAction username}>
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
                ·
                <small class="text-muted">
                    <a href={ShowPostAction (get #id post)}>
                        {getCreatedOnDayFormated today post :: Text}
                    </a>
                    {renderControlDropdown}
                </small>
            </div>
            <a class="text-reset" href={ShowPostAction (get #id post)}>
                <p class="p-1 p-md-3 post-text">
                    {get #body post}
                    {renderPostImage post}
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
        renderPostImage post =
                case get #postImageUrl post of
                        Nothing -> [hsx||]
                        Just _ -> [hsx|
                            <img class="w-100 rounded" src={fromMaybe ("#" :: Text) (get #postImageUrl post)} style="width:100%; height: auto;"/>
                        |]
        username = get #username post
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

getCreatedOnDayFormated today post = if diffDays today (get #createdOnDay post) < 7
        then cs $ formatTime defaultTimeLocale "%A" $ get #createdOnDay post
        else cs $ formatTime defaultTimeLocale "%B %d, %Y" $ get #createdOnDay post