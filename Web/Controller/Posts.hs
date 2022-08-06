module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Show
import Data.Time.Zones.All
import Data.Time
import Data.Time.Zones (utcToLocalTimeTZ, utcToLocalTimeTZ ,utcTZ)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (unpack)
import Data.Maybe (fromJust)
import Application.Helper.PostsQuery
import Admin.Controller.Prelude (Post'(bigPostTitle))
import IHP.Controller.Redirect (redirectBack)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Protolude (isJust)
import Data.Time.Calendar.WeekDate
import IHP.ControllerPrelude (validateField)
import Database.PostgreSQL.LibPQ (finish)
import qualified Admin.View.Prelude as IHP.ViewSupport
import Web.View.Users.FinishUserSetup
import Web.Controller.Users

instance Controller PostsController where
    action PostsAction = do
        ensureIsUser
        -- TODO: Delete this action, or update it to work
        -- pagination/like button
        --posts <- query @Post |> fetch
        --    >>= collectionFetchRelated #userId

        posts :: [PostWithMeta] <- fetchPostsWithMeta currentUserId 0 10
        day <- getUserDay $ get #timezone currentUser
        todaysPost <- getDailyPost currentUserId day
        let users = []

        let likes = []
        let page = Nothing
        let newPost = newRecord
        let showBigPostLink = False
        today <- getUserDay $ get #timezone currentUser
        render IndexView { .. }

    action FollowedPostsAction { page } = showPostIndex page newRecord

    action NewPostAction = do
        ensureIsUser
        day <- getUserDay $ get #timezone currentUser
        dailyPost <- getDailyPost currentUserId day
        let post = newRecord
        render NewView { .. }

    action ShowPostAction { postId } = showPost postId (newRecord @Comment |> set #postId postId)

    action ShowPostForDayAction { username, day } = do
        ensureIsUser
        user <- query @User |> filterWhere (#username, username) |> fetchOneOrNothing
        case (user, parseDay day) of
            (Just user, Just dayParsed) -> do
                post <- query @Post
                    |> filterWhere (#userId, get #id user)
                    |> filterWhere (#createdOnDay, dayParsed)
                    |> fetchOneOrNothing
                    -- TODO: This statement should be simplifed some how
                    >>= \case
                        Just post -> pure <$> fetchRelated #userId post
                        Nothing -> pure Nothing

                case post of
                    Nothing -> do
                        setErrorMessage "Couldn't find that post"
                        redirectTo $ FollowedPostsAction Nothing
                    Just post -> redirectTo ShowPostAction { postId = get #id post }
            _ -> do
                setErrorMessage "Couldn't find that post"
                redirectTo $ FollowedPostsAction Nothing
        where
            parseDay = parseTimeM False defaultTimeLocale "%-Y-%-m-%d" . Data.Text.unpack

    action CreatePostAction = do
        ensureIsUser
        day <- getUserDay $ get #timezone currentUser
        dailyPost <- getDailyPost currentUserId day

        when (isJust dailyPost) $ do
            setErrorMessage "You can only have one post per day"
            redirectTo $ FollowedPostsAction Nothing

        let isProUser = get #isPro currentUser


        let fileContent :: Text = fileOrNothing "postImageUrl"
                        |> fromMaybe (error "no file given")
                        |> get #fileContent
                        |> cs
        let theyUploadedAnImage = not $ isEmpty fileContent

        let postImageUploadSettings = uploadToStorageWithOptions $ def {
            preprocess = applyImageMagick "jpg" ["-sampling-factor", "4:2:0", "-strip", "-gravity", "north", "-quality", "85", "-interlace", "JPEG" ]
        }

        newRecord @Post
            |> set #userId currentUserId
            |> set #createdOnDay day
            |> set #userTimezoneSnapshot (get #timezone currentUser)
            |> (buildPost theyUploadedAnImage)
            |> (if (isProUser) then (postImageUploadSettings #postImageUrl) else pure)
            >>= ifValid \case
                Left post -> do
                    showPostIndex Nothing post
                Right post -> do
                    post <- post |> createRecord
                    redirectTo $ FollowedPostsAction Nothing

    action CreateBigPostAction = do
        ensureIsUser
        day <- getUserDay $ get #timezone currentUser
        dailyPost <- getDailyPost currentUserId day

        -- Check if today is Friday, Saturday, or Sunday
        let isFridayOrWeekend = checkIfFridayOrWeekend day

        -- Sanitize the post content
        let bigPostBody' :: Maybe Text = paramOrNothing "bigPostBody"
        let bigPostBody = fmap sanitizeBalance bigPostBody'

        -- Only allow the user to create one big post a weekend
        bigPostThisWeekend <- didTheCurrentUserPostAWeekendBigPost day

        when (isFridayOrWeekend && bigPostThisWeekend) $ do
            setErrorMessage "You can only create one big post a weekend"
            redirectBack

        newRecord @Post
            |> set #userId currentUserId
            |> set #createdOnDay day
            |> set #userTimezoneSnapshot (get #timezone currentUser)
            |> set #isBigPost True
            |> set #bigPostBody bigPostBody
            |> fill @'["body", "bigPostTitle"]
            |> validateField #body (hasMaxLength 280
                |> withCustomErrorMessage "Sub titles must be less than 280 characters")
            |> validateField #bigPostTitle nonEmptyAndNotNothing
            |> validateField #bigPostTitle (hasMaxLength 100 . fromMaybe ""
                |> withCustomErrorMessage "Titles must be less than 100 characters")
            |> validateField #body nonEmpty
            |> validateField #bigPostBody nonEmptyAndNotNothing
            |> ifValid \case
                Left post -> do
                    render NewView { .. }
                Right post -> do
                    unless (isNothing dailyPost) $ do
                        setErrorMessage "You already created a post today"
                        render NewView { .. }
                    unless isFridayOrWeekend $ do
                        setErrorMessage "You can only create a big post on Friday, Saturday or Sunday"
                        render NewView { .. }
                    post <- post |> createRecord
                    redirectTo $ FollowedPostsAction Nothing
        where
            nonEmptyAndNotNothing :: Maybe Text -> ValidatorResult
            nonEmptyAndNotNothing (Just "") = Failure "This field cannot be empty"
            nonEmptyAndNotNothing Nothing = Failure "This field cannot be empty"
            nonEmptyAndNotNothing _ = Success

    action DeletePostAction { postId } = do
        ensureIsUser
        post <- fetch postId
        accessDeniedUnless (get #userId post == currentUserId)
        -- Delete all the likes from the post
        sqlExec "DELETE FROM likes WHERE post_id = ?" (Only (get #id post))
        -- Delete all the comments from the post
        sqlExec "DELETE FROM comments WHERE post_id = ?" (Only (get #id post))
        -- Delete all the notifications about the post
        -- TODO: Don't delete the notfictions, just mark them as deleted somehow
        sqlExec "DELETE FROM notifications WHERE post_id = ?" (Only (get #id post))
        -- Delete the post
        deleteRecord post
        redirectTo $ FollowedPostsAction Nothing

buildPost theyUploadedAnImage post = post
    |> fill @'["body", "postImageUrl"]
    |> validateField #body (hasMaxLength 280
        |> withCustomErrorMessage "Post must be less than 280 characters")
    |> validateField #body (validatePostHasEitherBodyOrImage theyUploadedAnImage)


validatePostHasEitherBodyOrImage theyUploadedAnImage body =
        if (body == "" && not theyUploadedAnImage)
            then Failure "Your post is empty"
            else Success

-- (body = ""     && image is false) -- true
-- (body is false && image is false) -- true

getDailyPost :: (?modelContext :: ModelContext) => Id User -> Day -> IO (Maybe Post)
getDailyPost userId day = query @Post
            |> filterWhere (#userId, userId)
            |> filterWhere (#createdOnDay, day)
            |> fetchOneOrNothing

showPostIndex :: (?context::ControllerContext, ?modelContext::ModelContext, ?theAction::controller)
    => Maybe Int -- ^ Page number
    -> Post      -- ^ The potential new post. We pass it in because it may have validation errors. 
    -> IO ()
showPostIndex page newPost = do
    ensureIsUser
    unless (get #isSetup currentUser) finishUserAccountSetup
    let pageSize = 50
    let skip = if isJust page
                    then fromJust page*pageSize
                    else 0

    posts :: [PostWithMeta] <- fetchPostsWithMeta currentUserId skip pageSize

    likes <- query @Like
                |> filterWhere (#userId, currentUserId)
                |> filterWhereIn (#postId, ids posts)
                |> fetch

    day <- getUserDay $ get #timezone currentUser
    todaysPost <- getDailyPost currentUserId day

    let isFridayOrWeekend = checkIfFridayOrWeekend day
    showBigPostLink <- (&& isPro currentUser) . (&& isFridayOrWeekend) . not <$> didTheCurrentUserPostAWeekendBigPost day

    today <- getUserDay $ get #timezone currentUser
    render IndexView { .. }


showPost :: (?context::ControllerContext, ?modelContext::ModelContext, ?theAction::controller)
    => Id Post -- ^ The id of the post you want to show
    -> Comment -- ^ The potential new comment. We pass it in because it may have validation errors.
    -> IO ()
showPost postId newComment = do
    post <- fetchSinglePostWithMeta postId
    case post of
        [] -> redirectTo $ FollowedPostsAction Nothing
        (post:_) -> do
            (commentsQuery, commentspagination) <- query @Comment
                            |> filterWhere (#postId, postId)
                            |> orderByDesc #createdAt
                            |> paginateWithOptions
                                (defaultPaginationOptions
                                    |> set #maxItems 10)

            comments <- commentsQuery
                            |> fetch
                            >>= collectionFetchRelated #userId

            likes <- query @Like
                        |> filterWhere (#postId, postId)
                        |> fetch
                        >>= collectionFetchRelated #userId

            like <- case currentUserOrNothing of
                Nothing -> pure Nothing -- there is no current user
                Just _  -> query @Like
                        |> filterWhere (#userId, currentUserId)
                        |> filterWhere (#postId, postId)
                        |> fetchOneOrNothing

            let isLiked = not $ null like

            user <- fetch (get #userId post)

            -- Is the current user following this user?
            follow <- case currentUserOrNothing of
                Nothing -> pure Nothing -- there is no current user
                Just _  -> query @UserFollow
                                |> filterWhere (#followerId, currentUserId)
                                |> filterWhere (#followedId, get #id user)
                                |> fetchOneOrNothing
            let followed = not $ null follow

            -- How many followers does this user have?
            followerCount :: Int <- query @UserFollow
                |> filterWhere (#followedId, get #id user)
                |> fetchCount
                -- A user always follows themselves, so we subtract one
                |> fmap pred

            today <- getUserDay $ get #timezone user
            render ShowView { .. }


checkIfFridayOrWeekend :: Day -> Bool
checkIfFridayOrWeekend day =
    let (year, week, dayOfWeek) = toWeekDate day
    in dayOfWeek == 5 || dayOfWeek == 6 || dayOfWeek == 7


didTheCurrentUserPostAWeekendBigPost day = do
        bigPostThisWeekendCount :: Int <- query @Post
            |> filterWhere (#isBigPost, True)
            |> queryOr
              (queryOr
                (filterWhere (#createdOnDay, addDays 1 day))     -- If the user created a post tomorrow
                                                                 -- we have to check tomorrow because the 
                                                                 -- user could have changed his/her timezone
                (filterWhere (#createdOnDay, addDays (-1) day))) -- If the user created a post yesterday
              (queryOr
                (filterWhere (#createdOnDay, addDays (-2) day))   -- If the user created a post two days ago
                (filterWhere (#createdOnDay, addDays (-3) day)))  -- If the user created a post three days ago
            |> filterWhere (#userId, currentUserId)
            |> fetchCount

        return $ bigPostThisWeekendCount > 0