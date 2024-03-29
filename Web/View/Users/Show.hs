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
                        , last30DaysPosts :: [Day]
                        , last30DaysRange :: [Day]
                        , today :: Day
                        }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div class="yosemite-window text-center p-3">
            <h1>{get #username user}</h1>
            <img class="border rounded-circle mx-auto" src={picturePath (get #pictureUrl user)} style="width:100px; height: 100px"/>
            <div class="d-flex justify-content-center">
                <div class="pr-2"><span class="follower-count">{followerCount}</span> <span class="text-muted pl-1">Followers</span></div>
                <div class="">{postCount} <span class="text-muted pl-1">Posts</span></div>
            </div>
            <div class="pt-3">
                {followButton}
            </div>
            <div class="bio pt-3">
                {get #bio user}
            </div>
            <div class="float-center">
                <div class="contributions justify-content-center">
                    {forEach last30DaysRange (createBlock last30DaysPosts)}
                </div>
            </div>
        </div>
        <div class="my-3 p-md-3">
            {forEach (posts) (\post -> (Web.View.Posts.Show.renderPost today (isPostLiked post likes) post))}
        </div>
    |]
        where
            isPostLiked post likes = get #id post `elem` fmap (get #postId) likes
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

createBlock last30DaysPosts aday = [hsx|
    <div style={"background: " ++ (chooseBlockColor last30DaysPosts aday) ++";"} class="block"></div>
|]

chooseBlockColor last30DaysPosts aday = if aday `elem` last30DaysPosts
                                    then "#007bff" :: Text
                                    else "#D6EAFF"
