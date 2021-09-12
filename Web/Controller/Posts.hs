module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
import Data.Time.Zones.All
import Data.Time
import Data.Time.Zones (utcToLocalTimeTZ, utcToLocalTimeTZ ,utcTZ)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (unpack)
import Data.Maybe (fromJust)
import Application.Helper.PostsQuery

instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do
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
        render IndexView { .. }

    action FollowedPostsAction { page } = do
        let pageSize = 10
        let skip = if isJust page
                        then (fromJust page)*pageSize
                        else 0

        posts :: [PostWithMeta] <- fetchPostsWithMeta currentUserId skip pageSize

        likes <- query @Like
                    |> filterWhere (#userId, currentUserId)
                    |> filterWhereIn (#postId, ids posts)
                    |> fetch

        day <- getUserDay $ get #timezone currentUser
        todaysPost <- getDailyPost currentUserId day

        render IndexView { .. }

    action NewPostAction = do
        day <- getUserDay $ get #timezone currentUser
        dailyPost <- getDailyPost currentUserId day
        case dailyPost of
            Just _ -> do
                setErrorMessage "You already created a post today"
                redirectTo $ FollowedPostsAction Nothing
            Nothing -> do
                let post = newRecord
                render NewView { .. }

    action ShowPostAction { postId } = do
        post <- fetchSinglePostWithMeta postId
        case post of
            [] -> redirectTo $ FollowedPostsAction Nothing
            (post:_) -> do
                (commentsQuery, commentspagination) <- query @Comment
                                |> filterWhere (#postId, postId)
                                |> orderByDesc (#createdAt)
                                |> paginateWithOptions
                                    (defaultPaginationOptions
                                        |> set #maxItems 10)

                comments <- commentsQuery
                                |> fetch
                                >>= collectionFetchRelated #userId

                like <- query @Like
                            |> filterWhere (#userId, currentUserId)
                            |> filterWhere (#postId, postId)
                            |> fetchOneOrNothing

                let isLiked = not $ null like

                render ShowView { .. }

    action ShowPostForDayAction { username, day } = do
        user <- (query @User |> filterWhere (#username, username) |> fetchOneOrNothing)
        case (user, parseDay day) of
            (Just user, Just dayParsed) -> do
                post <- query @Post
                    |> filterWhere (#userId, get #id user)
                    |> filterWhere (#createdOnDay, dayParsed)
                    |> fetchOneOrNothing
                    -- TODO: This statement should be simplifed some how
                    >>= \case
                        Just post -> fmap pure $ fetchRelated #userId post
                        Nothing -> pure Nothing

                case post of
                    Nothing -> do
                        setErrorMessage "Couldn't find that post"
                        redirectTo $ FollowedPostsAction Nothing
                    Just post -> redirectTo $ ShowPostAction { postId = (get #id post) }
            _ -> do
                setErrorMessage "Couldn't find that post"
                redirectTo $ FollowedPostsAction Nothing
        where
            parseDay = parseTimeM False defaultTimeLocale "%-Y-%-m-%d" . Data.Text.unpack

    action EditPostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (get #userId post == currentUserId)
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (get #userId post == currentUserId)
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    action CreatePostAction = do
        day <- getUserDay $ get #timezone currentUser
        dailyPost <- getDailyPost currentUserId day
        case dailyPost of
            Just _ -> do
                setErrorMessage "You already created a post today"
                redirectTo $ FollowedPostsAction Nothing
            Nothing -> do
                newRecord @Post
                    |> set #userId currentUserId
                    |> set #createdOnDay day
                    |> set #userTimezoneSnapshot (get #timezone currentUser)
                    |> buildPost
                    |> ifValid \case
                        Left post -> redirectTo $ FollowedPostsAction Nothing
                        Right post -> do
                            post <- post |> createRecord
                            setSuccessMessage "Post created"
                            redirectTo $ FollowedPostsAction Nothing

    action DeletePostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (get #userId post == currentUserId)
        -- Delete all the likes from the post
        sqlExec "DELETE FROM likes WHERE post_id = ?" (Only (get #id post))
        -- Delete all the comments from the post
        sqlExec "DELETE FROM comments WHERE post_id = ?" (Only (get #id post))
        -- Delete the post
        deleteRecord post
        redirectTo $ FollowedPostsAction Nothing

buildPost post = post
    |> fill @'["body"]
    |> validateField #body (hasMaxLength 280)
    |> validateField #body (nonEmpty)

getDailyPost :: (?modelContext :: ModelContext) => Id User -> Day -> IO (Maybe Post)
getDailyPost userId day = query @Post
            |> filterWhere (#userId, userId)
            |> filterWhere (#createdOnDay, day)
            |> fetchOneOrNothing