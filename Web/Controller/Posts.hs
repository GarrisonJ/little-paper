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

instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do
        posts <- query @Post |> fetch
            >>= collectionFetchRelated #userId

        render IndexView { .. }


    action FollowedPostAction = do
        posts <- query @Post
                |> innerJoin @UserFollow (#userId, #userId)
                |> filterWhereJoinedTable @UserFollow (#followerId, currentUserId)
                |> fetch
            >>= collectionFetchRelated #userId


        render IndexView { .. }

    action NewPostAction = do
        day <- getUserDay $ get #timezone currentUser
        dailyPost <- getDailyPost currentUserId day
        case dailyPost of
            Just _ -> do
                setErrorMessage "You already created a post today"
                redirectTo PostsAction
            Nothing -> do
                let post = newRecord
                render NewView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }

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
                redirectTo PostsAction
            Nothing -> do
                newRecord @Post
                    |> set #userId currentUserId
                    |> set #createdOn day
                    |> buildPost
                    |> ifValid \case
                        Left post -> render NewView { .. }
                        Right post -> do
                            post <- post |> createRecord
                            setSuccessMessage "Post created"
                            redirectTo PostsAction

    action DeletePostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (get #userId post == currentUserId)
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

buildPost post = post
    |> fill @'["body"]

getUserDay :: Text -> IO (Day)
getUserDay preferedTimezone = do
    let preferedTimezoneParsed = fromMaybe utcTZ $ tzByName $ encodeUtf8 preferedTimezone
    currentLocalTime <- (utcToLocalTimeTZ preferedTimezoneParsed) <$> getCurrentTime
    return $ localDay currentLocalTime

getDailyPost :: (?modelContext :: ModelContext) => Id User -> Day -> IO (Maybe Post)
getDailyPost userId day = query @Post
            |> filterWhere (#userId, userId)
            |> filterWhere (#createdOn, day)
            |> fetchOneOrNothing