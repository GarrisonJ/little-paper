module Application.Helper.PostsQuery where

import IHP.Prelude
import IHP.ModelSupport
import Web.Controller.Prelude
import qualified Data.Time.Calendar
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import "string-interpolate" Data.String.Interpolate (i)

data PostWithMeta = PostWithMeta
    { id :: Id Post
    , body :: Text
    , createdOnDay :: Data.Time.Calendar.Day
    , userId :: Id User
    , createdOn :: UTCTime
    , userTimezoneSnapshot :: Text
    , isBigPost :: Bool
    , bigPostBody :: Maybe Text
    , bigPostTitle :: Maybe Text
    , postImageUrl :: Maybe Text
    , blurhashImagePlaceholder :: Maybe Text
    , postImageHeight :: Maybe Int
    , postImageWidth :: Maybe Int
    , username :: Text
    , pictureUrl :: Maybe Text
    , commentsCount :: Int
    , likesCount :: Int
    }
    deriving (Eq, Show)

instance FromRow PostWithMeta where
    fromRow =
        PostWithMeta
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

data PostCreatedOn = PostCreatedOn
    { createdOnDay :: Data.Time.Calendar.Day }
    deriving (Eq, Show)

instance FromRow PostCreatedOn where
    fromRow =
        PostCreatedOn
            <$> field

fetchPostsWithMeta :: (?modelContext :: ModelContext) => Id User -> Int -> Int -> IO [PostWithMeta]
fetchPostsWithMeta currentUserId skip pageSize = do
    trackTableRead "posts" -- This is needed when using auto refresh, so auto refresh knows that your action is accessing the posts table
    sqlQuery (bigPostQuery pageSize skip currentUserId) ()

fetchSinglePostWithMeta :: (?modelContext :: ModelContext) => Id Post -> IO [PostWithMeta]
fetchSinglePostWithMeta postId = do
    trackTableRead "posts" -- This is needed when using auto refresh, so auto refresh knows that your action is accessing the posts table
    sqlQuery (singlePostQuery) (Only postId)

fetchPostsWithMetaForProfle :: (?modelContext :: ModelContext) => Id User -> IO [PostWithMeta]
fetchPostsWithMetaForProfle profileUserId = do
    trackTableRead "posts" -- This is needed when using auto refresh, so auto refresh knows that your action is accessing the posts table
    sqlQuery (profileQuery profileUserId) ()

fetchLast30DaysPostsWithMetaForProfle :: (?modelContext :: ModelContext) => User -> IO [PostCreatedOn]
fetchLast30DaysPostsWithMetaForProfle user = do
    trackTableRead "posts" -- This is needed when using auto refresh, so auto refresh knows that your action is accessing the posts table
    let profileUserId = get #id user
    now <- getUserDay (get #timezone user)
    let nowMinus30DaysAgo = addDays (-30) now
    sqlQuery (profileLast30DaysQuery profileUserId nowMinus30DaysAgo) ()

singlePostQuery :: Query
singlePostQuery = [i|
    SELECT posts.*, 
           users.username, 
           users.picture_url, 
           (SELECT COUNT(*) FROM comments WHERE comments.post_id = posts.id) AS comments_count, 
           (SELECT COUNT(*) FROM likes WHERE likes.post_id = posts.id) AS likes_count
    FROM posts 
    INNER JOIN user_follows 
        ON posts.user_id = user_follows.followed_id 
    INNER JOIN users
        ON posts.user_id = users.id 
    WHERE users.is_confirmed = true 
        AND users.is_blocked = false
        AND users.is_setup = true
        AND posts.id = ? 
|]

bigPostQuery :: Int -> Int -> Id User -> Query
bigPostQuery pageSize skip userId = [i|
    SELECT posts.*, 
           users.username, 
           users.picture_url, 
           (SELECT COUNT(*) FROM comments WHERE comments.post_id = posts.id) AS comments_count, 
           (SELECT COUNT(*) FROM likes WHERE likes.post_id = posts.id) AS likes_count
    FROM posts 
    INNER JOIN user_follows 
        ON posts.user_id = user_follows.followed_id 
    INNER JOIN users
        ON posts.user_id = users.id 
    WHERE users.is_confirmed = true 
        AND users.is_setup = true 
        AND users.is_blocked = false
        AND user_follows.follower_id = '#{userId}'
    ORDER BY created_on DESC 
    LIMIT #{pageSize} 
    OFFSET #{skip} 
|]

profileQuery :: Id User -> Query
profileQuery userId = [i|
    SELECT posts.*, 
           users.username, 
           users.picture_url, 
           (SELECT COUNT(*) FROM comments WHERE comments.post_id = posts.id) AS comments_count, 
           (SELECT COUNT(*) FROM likes WHERE likes.post_id = posts.id) AS likes_count
    FROM posts 
    INNER JOIN users 
    ON posts.user_id = users.id 
    WHERE users.is_confirmed = true 
        AND users.is_setup = true 
        AND users.is_blocked = false
        AND posts.user_id = '#{userId}'
    ORDER BY created_on DESC 
|]

profileLast30DaysQuery :: Id User -> Day -> Query
profileLast30DaysQuery userId nowMinus30DaysAgo = [i|
    SELECT created_on_day
    FROM posts INNER JOIN users ON posts.user_id = users.id 
    WHERE posts.user_id = '#{userId}'
    AND users.is_confirmed = true 
    AND users.is_setup = true
    AND posts.created_on >= '#{nowMinus30DaysAgo}'
    ORDER BY created_on DESC 
|]