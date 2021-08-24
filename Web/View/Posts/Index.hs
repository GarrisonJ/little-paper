module Web.View.Posts.Index where
import Web.View.Prelude
import Web.View.Posts.Show

data IndexView = IndexView { posts :: [Include "userId" Post],
                             todaysPost :: Maybe Post,
                             page :: Maybe Int,
                             likes :: [Like]
                             }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {renderPostInput}
        <div>
            <div>{forEach posts renderPost}</div>
            <a  href={FollowedPostsAction prevPage} aria-label="Previous">
                {leftArrow}
            </a>
            <a href={FollowedPostsAction nextPage} aria-label="Next">
                {rightArrow}
            </a>

        </div>
    |]
        where
            rightArrow = [hsx|<span class="display-4">→</span>|]
            leftArrow = [hsx|<span class="display-4">←</span>|]
            nextPage = if isJust page then (fmap succ page) else Just 1
            prevPage = case page of
                        Nothing -> Nothing
                        Just 0  -> Just 0
                        _       -> fmap pred page
            renderPostInput = case todaysPost of
                    Just p -> [hsx|<h3>You posted something today. Nice job!</h3>|]
                    Nothing -> [hsx|
                                <div class="card yosemite-window">
                                    <div class="card-header border-light">You have't posted today!</div>
                                    <div class="card-body border-light">
                                        {renderPostForm (newRecord :: Post)}
                                    </div>
                                </div>
                            |]


            isPostLiked post likes = (get #id post) `elem` (fmap (get #postId) likes)
            renderPost post = [hsx|
                {Web.View.Posts.Show.renderPost (get #userId post) (isPostLiked post likes) post}
            |]

renderPostForm :: Post -> Html
renderPostForm post = formForWithOptions post postFormOptions [hsx|
    {(textareaField #body) { disableLabel = True }}
    {submitButton { label= "Submit", buttonClass="float-right send-message-button" } }
|]

postFormOptions :: FormContext (Post) -> FormContext (Post)
postFormOptions formContext =
    formContext
    |> set #formAction (pathTo CreatePostAction)
