module Web.View.Posts.Index where
import Web.View.Prelude
import Web.View.Posts.Show
import Application.Helper.PostsQuery

data IndexView = IndexView { posts :: [PostWithMeta],
                             todaysPost :: Maybe Post,
                             page :: Maybe Int,
                             likes :: [Like],
                             newPost :: Post
                             }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderPostInput}
        <div>
            <div>{forEach posts renderPost}</div>
            {renderPreviousArrow}
            {renderNextArrow}
        </div>
    |]
        where
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
                                    <div class="card-header border-light">You haven't posted today!</div>
                                    <div class="card-body border-light">
                                        {renderPostForm newPost}
                                    </div>
                                </div>
                            |]

            isPostLiked post likes = get #id post `elem` fmap (get #postId) likes
            renderPost post = [hsx|
                {Web.View.Posts.Show.renderPost (isPostLiked post likes) post}
            |]

renderPostForm :: Post -> Html
renderPostForm post = formForWithOptions post postFormOptions [hsx|
    {(textareaField #body) { disableLabel = True }}
    {submitButton { label= "Submit", buttonClass="float-right send-message-button" } }
|]

postFormOptions :: FormContext Post -> FormContext Post
postFormOptions formContext =
    formContext
    |> set #formAction (pathTo CreatePostAction)
