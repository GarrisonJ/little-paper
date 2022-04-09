module Web.View.Posts.Index where
import Web.View.Prelude
import Web.View.Posts.Show
import Application.Helper.PostsQuery

data IndexView = IndexView { posts :: [PostWithMeta]
                            , todaysPost :: Maybe Post
                            , page :: Maybe Int
                            , likes :: [Like]
                            , newPost :: Post
                            , showBigPostLink :: Bool
                            , today :: Day
                            }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderPostInput}
        <div>
            <div>{forEach posts renderPost}</div>
            {followMorePeople}
            {renderPreviousArrow}
            {renderNextArrow}
        </div>
    |]
        where
            followMorePeople = if length posts <= 1
                then [hsx|
                    <div class="d-flex p-3 text-center justify-content-center yosemite-window">
                        <div class="w-100">
                            <span class="p-2">
                                You should follow more people!
                            </span>
                            <div class="p-2">
                                <a href={UsersAction}>
                                    Find People
                                </a>
                            </div>
                        </div>
                    </div>
                |]
                else [hsx||]
            renderPreviousArrow = case page of
                                        Nothing -> [hsx||]
                                        Just 0 -> [hsx||]
                                        _ -> [hsx|
                                            <a href={FollowedPostsAction prevPage} class="" aria-label="Previous">
                                                {leftArrow}
                                            </a>
                                        |]
            renderNextArrow = if null posts
                                then [hsx||]
                                else [hsx|
                                    <a href={FollowedPostsAction nextPage} class="float-right" aria-label="Next">
                                        {rightArrow}
                                    </a>
                                    |]

            rightArrow = [hsx|<span class="display-4">→</span>|]
            leftArrow = [hsx|<span class="display-4">←</span>|]
            nextPage = if isJust page then fmap succ page else Just 1
            prevPage = case page of
                        Nothing -> Nothing
                        Just 0  -> Just 0
                        _       -> fmap pred page
            renderPostInput = case todaysPost of
                    Just p -> [hsx|
                        <div class="card yosemite-window">
                            <div class="card-body">You posted something today. 🎉</div>
                        </div>
                    |]
                    Nothing -> [hsx|
                                <div class="card yosemite-window">
                                    <div class="card-body border-light">
                                        {renderPostForm showBigPostLink newPost}
                                    </div>
                                </div>
                            |]

            isPostLiked post likes = get #id post `elem` fmap (get #postId) likes
            renderPost post = [hsx|
                {Web.View.Posts.Show.renderPost today (isPostLiked post likes) post}
            |]

renderPostForm :: Bool -> Post -> Html
renderPostForm showBigPostLink post = formForWithOptions post postFormOptions [hsx|
    <div class="form-group" id="form-group-post_body">
        <textarea type="text" name="body" rows="4" maxlength="280" placeholder="Your post." id="post_body" class="form-control post-textarea">
        </textarea>
        <span class="position-absolute text-muted" style="right: 25px; bottom: 75px;" id="char-count">280</span>
    </div>
    {submitButton { label= "Submit", buttonClass="btn-block"} }
    {renderGoToNewPostLink}
|]
    where
        -- render go to new post if today is Friday, Saturday, or Sunday
        renderGoToNewPostLink = if showBigPostLink
                                    then [hsx|
                                        <a href={NewPostAction} class=" float-left">
                                            Post something more this weeked! 🎉 
                                        </a>
                                        |]
                                    else [hsx||]

postFormOptions :: FormContext Post -> FormContext Post
postFormOptions formContext =
    formContext
    |> set #formAction (pathTo CreatePostAction)
