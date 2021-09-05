module Application.Helper.PostsQuery where

import IHP.Prelude
import IHP.ModelSupport
import Web.Controller.Prelude
import qualified Data.Time.Calendar
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Query)

data PostWithMeta = PostWithMeta
    { id :: Id Post
    , body :: Text
    , createdOnDay :: Data.Time.Calendar.Day
    , userId :: Id User
    , createdOn :: UTCTime
    , userTimezoneSnapshot :: Text
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
    WHERE posts.id = ?
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
    WHERE user_follows.follower_id = '#{userId}'
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
    FROM posts INNER JOIN users ON posts.user_id = users.id 
    WHERE posts.user_id = '#{userId}'
    ORDER BY created_on DESC 
|]